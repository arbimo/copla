package copla.planning.causality.support

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.{constraints, CSP, CSPView}
import copla.constraints.meta.constraints._
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar
import copla.lang.model.core
import copla.planning.causality.{DecisionPending, SupportByExistingChange, SupportOption}

/** Enforces that the given support variable is supported by a changing from the given action. */
class SupportByAction(act: core.Action, supportVar: SupportVar) extends Constraint {

  override def variables(implicit csp: CSPView): Set[IVar] = Set(supportVar)

  private def isValidSupport(opt: SupportOption): Boolean = opt match {
    case SupportByExistingChange(change) if act.content.contains(change.ref) => true
    case DecisionPending                                                     => true
    case _                                                                   => false
  }

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    if (supportVar.dom.contains(DecisionPending))
      ConstraintSatisfaction.UNDEFINED
    else if (supportVar.dom.values.forall(isValidSupport))
      ConstraintSatisfaction.SATISFIED
    else {
      ConstraintSatisfaction.UNDEFINED
    }
  }

  override def propagate(event: Event)(implicit csp: CSPView): PropagationResult = {
    if (supportVar.domain.isEmpty) {
      Inconsistency
    } else if (!supportVar.dom.contains(DecisionPending) && supportVar.dom.values.forall(isValidSupport)) {
      Satisfied()
    } else {
      val removals = Domain(
        supportVar.dom.valuesWithIntRepresentation
          .filterNot(pair => isValidSupport(pair._1))
          .map(_._2)
          .toSet)

      if (removals.isEmpty)
        Undefined()
      else if (supportVar.domain.containedBy(removals))
        Inconsistency
      else
        Undefined(UpdateDomain(supportVar, supportVar.domain - removals))
    }
  }

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = ???

  override def toString: String = s"SupportByAction($act, $supportVar)"
}
