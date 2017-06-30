package copla.constraints.meta.decisions

import copla.constraints.meta.CSP
import copla.constraints.meta.variables.VarWithDomain

case class VarBinaryDecision(v: VarWithDomain) extends Decision {

  override def pending(implicit csp: CSP): Boolean = !v.domain.isSingleton

  override def options(implicit csp: CSP): Seq[DecisionOption] = {
    if (v.domain.isEmpty) {
      List()
    } else {
      val value = v.domain.values.head
      List(new DecisionConstraint(v === value), new DecisionConstraint(v =!= value))
    }
  }

  override def numOption(implicit csp: CSP): Int = v.domain.size
}
