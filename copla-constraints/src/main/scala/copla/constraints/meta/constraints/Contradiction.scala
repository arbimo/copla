package copla.constraints.meta.constraints

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar

/** An always violated constraint */
class Contradiction extends Constraint {

  override def variables(implicit csp: CSPView): Set[IVar] = Set()

  override def satisfaction(implicit csp: CSPView): Satisfaction = ConstraintSatisfaction.VIOLATED

  override def propagate(event: Event)(implicit csp: CSPView) = Inconsistency

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = new Tautology
}

object Contradiction {
  private val instance = new Contradiction
  def apply(): Contradiction = instance
}
