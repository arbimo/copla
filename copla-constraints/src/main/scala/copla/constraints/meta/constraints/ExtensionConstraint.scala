package copla.constraints.meta.constraints

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.CSP
import copla.constraints.meta.domains.ExtensionDomain
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.{IVar, IntVariable}

/** A constraint on N variables that must take as values one of the N-tuples in an ExtensionDomain*/
class ExtensionConstraint(_variables: Seq[IntVariable], extDomain: ExtensionDomain)
    extends Constraint {

  override def variables(implicit csp: CSP): Set[IVar] = _variables.toSet

  override def satisfaction(implicit csp: CSP): Satisfaction = {
    val domains = _variables.map(_.domain)
    if (domains.forall(_.isSingleton)) {
      val values = domains.map(_.values.head)
      if (extDomain.hasTuple(values))
        ConstraintSatisfaction.SATISFIED
      else
        ConstraintSatisfaction.VIOLATED
    } else {
      ConstraintSatisfaction.UNDEFINED
    }
  }

  override protected def _propagate(event: Event)(implicit csp: CSP): Unit = {
    val initialDomains    = _variables.map(_.domain)
    val restrictedDomains = extDomain.restrictedDomains(initialDomains)
    for (i <- _variables.indices) {
      if (restrictedDomains(i).isEmpty)
        throw new InconsistentBindingConstraintNetwork()
      else if (initialDomains(i).size > restrictedDomains(i).size)
        csp.updateDomain(_variables(i), restrictedDomains(i))
    }
  }

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = ???
}
