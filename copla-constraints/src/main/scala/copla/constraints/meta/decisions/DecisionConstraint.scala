package copla.constraints.meta.decisions

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints.Constraint

case class DecisionConstraint(constraint: Constraint) extends DecisionOption {
  override def enforceIn(csp: CSP): Unit = csp.post(constraint)

  override def negate(implicit csp: CSPView): DecisionOption = DecisionConstraint(constraint.reverse)
}
