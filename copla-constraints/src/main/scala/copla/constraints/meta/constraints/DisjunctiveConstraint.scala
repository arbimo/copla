package copla.constraints.meta.constraints

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints.ConstraintSatisfaction._
import copla.constraints.meta.domains.{Domain, SingletonDomain}
import copla.constraints.meta.events.{DomainReduced, Event, WatchedSatisfied, WatchedViolated}
import copla.constraints.meta.variables._

class DisjunctiveConstraint(val disjuncts: Seq[Constraint]) extends Constraint {

  val decisionVar = new IntVar(Domain(disjuncts.indices.toSet)) {
    override def toString: String = s"disjunctive-dec-var[${DisjunctiveConstraint.this}]"
  }

  override def variables(implicit csp: CSPView): Set[IVar] = Set(decisionVar)

  override def subconstraints(implicit csp: CSPView) = disjuncts

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val satisfactions = disjuncts.map(_.satisfaction)
    if (satisfactions.contains(SATISFIED))
      SATISFIED
    else if (satisfactions.contains(UNDEFINED))
      UNDEFINED
    else
      VIOLATED
  }

  override def propagate(event: Event)(implicit csp: CSPView) = {
    event match {
      case WatchedSatisfied(c) =>
        assert(c.isSatisfied)
        assert(decisionVar.domain.contains(disjuncts.indexOf(c)),
               "Decision var does not contain a satisfied constraint")
        if (decisionVar.boundTo(disjuncts.indexOf(c)))
          Satisfied()
        else
          Satisfied(UpdateDomain(decisionVar, Domain(disjuncts.indexOf(c))))
      case WatchedViolated(c) =>
        assert(c.isViolated)
        if (decisionVar.boundTo(disjuncts.indexOf(c)))
          Inconsistency
        else if (decisionVar.domain.contains(disjuncts.indexOf(c))) {
          Undefined(UpdateDomain(decisionVar, decisionVar.domain - disjuncts.indexOf(c)))
        } else {
          Undefined()
        }
      case DomainReduced(`decisionVar`) =>
        if (decisionVar.isBound) {
          val selectedConstraint = disjuncts(decisionVar.value)
          Undefined(Post(selectedConstraint))
        } else {
          Undefined()
        }
      case _ =>
        Undefined()
    }
  }

  override def toString = "(" + disjuncts.mkString(" || ") + ")"

  override def ||(c: Constraint) =
    new DisjunctiveConstraint(c :: disjuncts.toList)

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint =
    new ConjunctionConstraint(disjuncts.map(_.reverse))
}
