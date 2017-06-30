package copla.constraints.meta.constraints

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.CSP
import copla.constraints.meta.domains.SingletonDomain
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.{IVar, IntVariable}

class BindConstraint(val variable: IntVariable, val value: Int) extends Constraint {

  override def variables(implicit csp: CSP): Set[IVar] = Set(variable)

  override def satisfaction(implicit csp: CSP): Satisfaction =
    if (variable.domain.isSingleton && variable.domain.contains(value))
      ConstraintSatisfaction.SATISFIED
    else if (!variable.domain.contains(value))
      ConstraintSatisfaction.VIOLATED
    else
      ConstraintSatisfaction.UNDEFINED

  override protected def _propagate(event: Event)(implicit csp: CSP) {
    if (variable.domain.contains(value)) {
      if (!variable.domain.isSingleton)
        csp.updateDomain(variable, new SingletonDomain(value))
    } else {
      throw new InconsistentBindingConstraintNetwork()
    }
  }

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: NegBindConstraint = new NegBindConstraint(variable, value)

  override def toString = s"$variable === $value"
}
