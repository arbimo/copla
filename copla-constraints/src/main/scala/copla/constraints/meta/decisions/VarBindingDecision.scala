package copla.constraints.meta.decisions

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.variables.VarWithDomain

case class VarBindingDecision(variable: VarWithDomain) extends Decision {

  override def pending(implicit csp: CSPView): Boolean = !variable.domain.isSingleton

  override def options(implicit csp: CSPView): Seq[DecisionOption] = {
    variable.domain.values.toSeq.map(value => DecisionConstraint(variable === value))
  }

  override def numOptions(implicit csp: CSPView): Int = variable.domain.size
}
