package copla.constraints.meta.decisions

import copla.constraints.meta.CSP
import copla.constraints.meta.constraints.Constraint

case class DecisionConstraint(constraint: Constraint) extends DecisionOption {
  override def enforceIn(csp: CSP): Unit = csp.post(constraint)
}
