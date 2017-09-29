package copla.constraints.meta.decisions

import copla.constraints.meta.CSP
import copla.constraints.meta.variables.VarWithDomain

case class VarBinaryDecision(variable: VarWithDomain) extends Decision {

  override def pending(implicit csp: CSP): Boolean = !variable.domain.isSingleton

  override def options(implicit csp: CSP): Seq[DecisionOption] = {
    variable.domain.values.toSeq.map(value => DecisionConstraint(variable === value))
  }

  override def numOptions(implicit csp: CSP): Int = variable.domain.size
}
