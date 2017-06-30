package copla.constraints.meta.constraints

import copla.constraints.meta.CSP
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar

/** A constraint that is always satisfied */
class Tautology extends Constraint {

  override def variables(implicit csp: CSP): Set[IVar] = Set()

  override def satisfaction(implicit csp: CSP): Satisfaction = ConstraintSatisfaction.SATISFIED

  override protected def _propagate(event: Event)(implicit csp: CSP) {}

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = new Contradiction
}
