package copla.constraints.meta.constraints

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar

/** A constraint that is always satisfied */
class Tautology extends Constraint {

  override def variables(implicit csp: CSPView): Set[IVar] = Set()

  override def satisfaction(implicit csp: CSPView): Satisfaction = ConstraintSatisfaction.SATISFIED

  override def propagate(event: Event)(implicit csp: CSPView) = Satisfied()

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = new Contradiction
}
