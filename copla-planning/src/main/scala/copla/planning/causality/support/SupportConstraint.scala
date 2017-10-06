package copla.planning.causality.support

import copla.constraints.meta.CSPView
import copla.constraints.meta.constraints.ConstraintSatisfaction.{EVENTUALLY_SATISFIED, SATISFIED, UNDEFINED, VIOLATED}
import copla.constraints.meta.constraints._
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events._
import copla.constraints.meta.types.dynamics.DynamicType
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.IVar
import copla.planning.causality.{DecisionPending, SupportByActionInsertion, SupportByExistingChange, SupportOption}
import copla.planning.structures.{Change, Holds}

/** Constraint enforcing the given `holds` to be supported by a Change. */
class SupportConstraint(t: DynamicType[SupportOption], val holds: Holds)
    extends Constraint
    with WithData[SupportConstraintData] with slogging.LazyLogging {

  /** Variable denoting which supports are possible.
    *  Each value of the domain corresponds to a change in the CausalHandler */
  val supportVar       = new SupportVar(t, holds)
  private val decision = new SupportDecision(supportVar)

  /** Generates new constraint for each possible support (from the support var).
    * Mapping from support to constraints is maintained in the data structure SupportConstraintData.
    * Each of the constraint is then watched. */
  override def onPost(implicit csp: CSPView): Seq[OnPostChange] = {
    val d = new SupportConstraintData()
    (super.onPost :+ InitData(this, d) :+ AddDecision(decision)) ++ updatesFromDomain(d)
  }

  override def variables(implicit csp: CSPView): Set[IVar] = Set(supportVar)

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val directSupportSatisfactions = supportVar.typ.static.instances.collect {
        case SupportByExistingChange(c) => supportConstraintForChange(c).satisfaction
    }
    if(directSupportSatisfactions.contains(SATISFIED))
      SATISFIED
    else if(directSupportSatisfactions.contains(EVENTUALLY_SATISFIED))
      EVENTUALLY_SATISFIED
    else if(supportVar.domain.isEmpty)
      VIOLATED
    else
      UNDEFINED
  }

  override def propagate(event: Event)(implicit csp: CSPView): PropagationResult = {
    event match {
      case NewConstraint(c) =>
        assert1(c == this) // nothing to do, everything initialized in onPost
        Undefined()
      case DomainReduced(`supportVar`) =>
        if (supportVar.domain.isSingleton) {
          val domainValue = supportVar.domain.values.head
          val value       = supportVar.dom.values.head
          value match {
            case DecisionPending =>
              Undefined()
            case SupportByExistingChange(_) =>
              // actual support and decision was made (absent from domain), post the support constraint.
              val c = data.constraintOf(domainValue)
              if (c.isSatisfied)
                Satisfied(RetractDecision(decision))
              else
                Undefined(Post(c), Watch(c))
            case SupportByActionInsertion(_) =>
              sys.error(
                "Support constraints domain is reduced to an action insertion (without the decision pending marker).")
          }
        } else if (supportVar.domain.isEmpty) {
          Inconsistency
        } else {
          Undefined()
        }
      case DomainExtended(`supportVar`) =>
        Undefined(updatesFromDomain(data))
      case WatchedSatisfied(c) =>
        val d       = data
        val i       = d.indexOf(c)
        val support = t.static.intToInstance(i)
        support match {
          case SupportByExistingChange(_) =>
            Satisfied(RetractDecision(decision))
          case SupportByActionInsertion(aps) =>
            // ignore, even if it is satisfied it does not mean we have no other options
            Undefined()
          case DecisionPending =>
            sys.error("Problem: no constraint should map to DecisionPending")
        }
      case WatchedViolated(c) =>
        val d = data
        val i = d.indexOf(c)
        Undefined(UpdateDomain(supportVar, supportVar.domain - i))
    }
  }

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = ???

  /** Generates a new constriant for each domain element that is not tied to a constraint yet. */
  private def newConstraintsFromDomain(currentData: SupportConstraintData)(
      implicit csp: CSPView): Seq[(Int, Constraint)] = {
    val dom     = supportVar.domain
    val mapping = t.static
    dom.values.toSeq
      .filterNot(currentData.hasConstraintFor)
      .map(i => (i, mapping.intToInstance(i)))
      .collect {
        case (i, SupportByExistingChange(change)) =>
          (i, supportConstraintForChange(change))
        case (i, SupportByActionInsertion(aps)) =>
          (i, aps.potentialSupportConstraint(holds))
      }
  }

  private def updatesFromDomain(currentData: SupportConstraintData)(
      implicit csp: CSPView): Seq[OnPropagationChange with OnPostChange] = {
    val newConstraintsWithIndex = newConstraintsFromDomain(currentData)
    val dataUpdates = newConstraintsWithIndex.map {
      case (i, c) =>
        val updateFunction = (d: SupportConstraintData) => { d.put(i, c) }
        UpdateData(this, updateFunction)
    }
    val additionalWatches = newConstraintsWithIndex.map {
      case (_, c) => Watch(c)
    }
    dataUpdates ++ additionalWatches
  }

  private def supportConstraintForChange(c: Change): Constraint =
    if (holds.precedingChange)
      holds.fluent === c.fluent &&
      holds.value === c.value &&
      holds.persists.start >= c.persists.start &&
      holds.persists.end === c.persists.end
    else
      holds.fluent === c.fluent &&
      holds.value === c.value &&
      holds.persists.start >= c.persists.start &&
      holds.persists.end <= c.persists.end

  override def toString: String = s"support-constraint@[${holds.ref}]"
}
