package copla.constraints.meta.constraints

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.constraints.ConstraintSatisfaction.{SATISFIED, UNDEFINED, VIOLATED}
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.domains.ExtensionDomain
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.{IVar, IntVariable}

/** A constraint on N variables that must take as values one of the N-tuples in an ExtensionDomain*/
class ExtensionConstraint(_variables: Seq[IntVariable], extDomain: ExtensionDomain)
    extends Constraint {

  override def toString: String = s"ext-constraint([${_variables.mkString(",")}] <=> $extDomain"

  override def variables(implicit csp: CSPView): Set[IVar] = _variables.toSet

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val domains = _variables.map(_.domain)
    if (domains.forall(_.isSingleton)) {
      val values = domains.map(_.values.head)
      if (extDomain.hasTuple(values))
        SATISFIED
      else
        VIOLATED
    } else {
      UNDEFINED
    }
  }

  override def propagate(event: Event)(implicit csp: CSPView) = {
    val initialDomains    = _variables.map(_.domain)
    val restrictedDomains = extDomain.restrictedDomains(initialDomains)
    if (restrictedDomains.exists(_.isEmpty)) {
      Inconsistency
    } else {
      val changes =
        for (i <- _variables.indices if initialDomains(i).size > restrictedDomains(i).size)
          yield UpdateDomain(_variables(i), restrictedDomains(i))

      // build a lookahead containing all changes to apply to determine whether the constraint will be satisfied
      val lookahead = csp ++ changes
      satisfaction(lookahead) match {
        case SATISFIED => Satisfied(changes)
        case VIOLATED  => Inconsistency
        case _ => Undefined(changes)
      }
    }
  }

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = ???
}
