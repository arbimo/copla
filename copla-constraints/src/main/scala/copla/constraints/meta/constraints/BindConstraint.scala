package copla.constraints.meta.constraints

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.domains.{Domain, SingletonDomain}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.{IVar, IntVariable}

class BindConstraint(val variable: IntVariable, val value: Int) extends Constraint {

  override def variables(implicit csp: CSPView): Set[IVar] = Set(variable)

  override def satisfaction(implicit csp: CSPView): Satisfaction =
    if (variable.domain.isSingleton && variable.domain.contains(value))
      ConstraintSatisfaction.SATISFIED
    else if (!variable.domain.contains(value))
      ConstraintSatisfaction.VIOLATED
    else
      ConstraintSatisfaction.UNDEFINED

  override def propagate(event: Event)(implicit csp: CSPView): PropagationResult = {
    if (variable.boundTo(value))
      Satisfied()
    else if (variable.domain.contains(value))
      Satisfied(UpdateDomain(variable, Domain(value)))
    else
      Inconsistency
  }

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: NegBindConstraint = new NegBindConstraint(variable, value)

  override def toString = s"$variable === $value"
}
