package copla.constraints.meta.variables

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints._
import copla.constraints.meta.domains.Domain

object IntVariable {
  var intVariableCounter = 0
  def next()             = { intVariableCounter += 1; intVariableCounter - 1 }
}

trait IVar {

  def unaryConstraints: Seq[Constraint] = Nil

  // constraints shorthand
  def ===(other: IVar): EqualityConstraint = (this, other) match {
    case (v1: IntVariable, v2: IntVariable) => new VariableEqualityConstraint(v1, v2)
    case (v1: VariableSeq, v2: VariableSeq) => new VariableSeqEqualityConstraint(v1, v2)
    case _ =>
      throw new RuntimeException(
        s"Unknown instantiation of equality constraints for variables: $this and $other")
  }

  def =!=(other: IVar): InequalityConstraint = (this, other) match {
    case (v1: IntVariable, v2: IntVariable) => new VariableInequalityConstraint(v1, v2)
    case (v1: VariableSeq, v2: VariableSeq) => new VariableSeqInequalityConstraint(v1, v2)
    case _ =>
      throw new RuntimeException(
        s"Unknown instantiation of equality constraints for variables: $this and $other")
  }
}

/** Denotes a variable whose domain can be retrieved and represented explicitly */
trait VarWithDomain extends IVar {
  def domain(implicit csp: CSPView): Domain

  def isBound(implicit csp: CSPView) = domain.isSingleton

  def boundTo(value: Int)(implicit csp: CSPView) = domain.isSingleton && domain.contains(value)

  def value(implicit csp: CSPView) = {
    require(domain.isSingleton, "Can only request the value of a bound variable")
    domain.values.head
  }

  /** If true, a new decision will be generated when the variable is added to a CSP. */
  def isDecisionVar: Boolean

  def ===(value: Int): Constraint

  def =!=(value: Int): Constraint
}

abstract class IntVariable(val ref: Option[Any]) extends VarWithDomain {
  private val id = IntVariable.next()

  def initialDomain(implicit csp: CSPView): Domain // todo: check if the csp parameter can be removed

  def domain(implicit csp: CSPView): Domain = csp.dom(this)

  /** By default, any IntVar is a decision variable */
  override def isDecisionVar: Boolean = true

  override def ===(value: Int): BindConstraint = new BindConstraint(this, value)

  override def =!=(value: Int): NegBindConstraint = new NegBindConstraint(this, value)

  override def toString = ref match {
    case Some(x) => s"$x"
    case None    => s"v$id"
  }
}

class IntVar(_initialDomain: Domain, ref: Option[Any] = None) extends IntVariable(ref) {

  def initialDomain(implicit csp: CSPView): Domain = _initialDomain

}

class BooleanVariable(initialDomain: Domain, ref: Option[Any]) extends IntVar(initialDomain, ref) {

  def this(initialDomain: Domain) = this(initialDomain, None)

  def isTrue(implicit csp: CSPView): Boolean =
    domain.isSingleton && domain.contains(1)

  def isFalse(implicit csp: CSPView): Boolean =
    domain.isSingleton && domain.contains(0)
}

class VariableSeq(val variables: Seq[IVar], ref: Option[Any] = None) extends IVar {

  override def toString: String = ref match {
    case Some(x) => s"$x"
    case None    => s"(${variables.map(_.toString).mkString(", ")})"
  }

}
