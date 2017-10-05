package copla.constraints.meta.constraints

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.{IVar, IntVariable, VariableSeq}

trait InequalityConstraint extends Constraint {

  def v1: IVar
  def v2: IVar

  override def toString = s"$v1 =!= $v2"
}

class VariableInequalityConstraint(override val v1: IntVariable, override val v2: IntVariable)
    extends InequalityConstraint {

  override def variables(implicit csp: CSPView): Set[IVar] = Set(v1, v2)

  override def propagate(event: Event)(implicit csp: CSPView) = {
    val d1 = csp.dom(v1)
    val d2 = csp.dom(v2)

    if (d1.isSingleton && d2.isSingleton && d1 == d2) {
      Inconsistency
    } else if (d1.emptyIntersection(d2)) {
      Satisfied()
    } else if (d1.isSingleton) {
      Satisfied(UpdateDomain(v2, d2 - d1))
    } else if (d2.isSingleton) {
      Satisfied(UpdateDomain(v1, d1 - d2))
    } else {
      Undefined()
    }
  }

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val d1 = csp.dom(v1)
    val d2 = csp.dom(v2)

    if (d1.emptyIntersection(d2))
      ConstraintSatisfaction.SATISFIED
    else if (d1.isSingleton && d2.isSingleton && d1 == d2)
      ConstraintSatisfaction.VIOLATED
    else if(csp.domains.enforcedEqual(v1, v2))
      ConstraintSatisfaction.EVENTUALLY_VIOLATED
    else
      ConstraintSatisfaction.UNDEFINED
  }

  override def reverse: Constraint = new VariableEqualityConstraint(v1, v2)
}

class VariableSeqInequalityConstraint(override val v1: VariableSeq, override val v2: VariableSeq)
    extends DisjunctiveConstraint(v1.variables.zip(v2.variables).map(p => p._1 =!= p._2))
    with InequalityConstraint {

  require(v1.variables.size == v2.variables.size)

  override def reverse: Constraint = new VariableSeqEqualityConstraint(v1, v2)
}
