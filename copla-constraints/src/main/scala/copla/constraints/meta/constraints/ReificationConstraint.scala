package copla.constraints.meta.constraints

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints.ConstraintSatisfaction._
import copla.constraints.meta.domains.{BooleanDomain, Domain}
import copla.constraints.meta.events.{Event, NewConstraint, WatchedSatisfied, WatchedViolated}
import copla.constraints.meta.variables.ReificationVariable
import copla.constraints.meta.variables.IVar

class ReificationConstraint(val reiVar: ReificationVariable, val constraint: Constraint)
    extends Constraint {
  require(reiVar.constraint == constraint)

  override def variables(implicit csp: CSPView): Set[IVar] = Set(reiVar)

  override def subconstraints(implicit csp: CSPView) = Set(constraint)

  override def propagate(event: Event)(implicit csp: CSPView): PropagationResult = {
    event match {
      case event: UpdateDomain =>
        assert(event.variable == reiVar)
        if (reiVar.boundTo(1)) {
          // reification var to true
          if (constraint.isSatisfied)
            Satisfied()
          else if (constraint.isViolated)
            Inconsistency
          else
            Undefined(Post(constraint))
        } else if (reiVar.boundTo(0)) {
          // reification var to false
          if (constraint.isViolated)
            Satisfied()
          else if (constraint.isSatisfied)
            Inconsistency
          else
            Undefined(Post(constraint.reverse))
        } else if (reiVar.domain.isEmpty) {
          // reification var is neither nor false
          Inconsistency
        } else {
          // reification var is true or false
          Undefined()
        }
      case WatchedSatisfied(c) =>
        assert(c == constraint)
        if (reiVar.domain.contains(1))
          Satisfied(UpdateDomain(reiVar, Domain(1)))
        else
          Inconsistency
      case WatchedViolated(c) =>
        assert(c == constraint)
        if (reiVar.domain.contains(0))
          Satisfied(UpdateDomain(reiVar, Domain(0)))
        else
          Inconsistency
      case _ =>
        PropagationResult.from(satisfaction)
    }
  }

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    if (reiVar.boundTo(1)) {
      constraint.satisfaction
    } else if (reiVar.boundTo(0)) {
      constraint.reverse.satisfaction
    } else {
      assert(!reiVar.domain.isEmpty, "Reification variable should never have an empty domain")
      UNDEFINED
    }
  }

  override def toString = s"[$reiVar] <=> $constraint"

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = ???
}
