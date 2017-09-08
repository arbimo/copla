package copla.planning.causality.support

import copla.constraints.meta.types.dynamics.{DynTypedVariable, DynamicType}
import copla.planning.causality.SupportOption
import copla.planning.structures.Holds

class SupportVar(t: DynamicType[SupportOption], val target: Holds) extends DynTypedVariable[SupportOption](t) {

  override def isDecisionVar = false

  override def toString = s"support-var@[$target]"
}
