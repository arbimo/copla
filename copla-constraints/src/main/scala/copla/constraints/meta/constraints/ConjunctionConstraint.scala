package copla.constraints.meta.constraints

import copla.constraints.meta.constraints.ConstraintSatisfaction._
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar

class ConjunctionConstraint(val constraints: Seq[Constraint]) extends Constraint {

  override def onPost(implicit csp: CSPView) = {
    super.onPost ++ constraints.map(c => Post(c))
  }

  override def variables(implicit csp: CSPView): Set[IVar] = Set()

  override def subconstraints(implicit csp: CSPView) = constraints

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val satisfactions = constraints.map(_.satisfaction)
    if (satisfactions.forall(_ == SATISFIED))
      SATISFIED
    else if (satisfactions.forall(s => s == SATISFIED || s == EVENTUALLY_SATISFIED))
      EVENTUALLY_SATISFIED
    else if (satisfactions.contains(VIOLATED))
      VIOLATED
    else if (satisfactions.contains(EVENTUALLY_VIOLATED))
      EVENTUALLY_VIOLATED
    else
      ConstraintSatisfaction.UNDEFINED
  }

  override def propagate(event: Event)(implicit csp: CSPView) = satisfaction match {
    case SATISFIED => Satisfied()
    case VIOLATED  => Inconsistency
    case _ => Undefined()
  }

  override def toString = "(" + constraints.mkString(" && ") + ")"

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint =
    new DisjunctiveConstraint(constraints.map(_.reverse))
}
