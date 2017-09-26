package copla.lang.model

import copla.lang.model

package object core {

  type CoreModel = Seq[InModuleBlock]

  sealed trait Block         extends full.Block with InCore
  sealed trait InModuleBlock extends full.InModuleBlock with InCore
  sealed trait Statement     extends InModuleBlock with full.Statement

  sealed trait InCore

  /** A block wrapping other blocks pertaining to the same scope. */
  sealed trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  sealed trait SymExpr {
    def typ: Type
  }
  trait TimedSymExpr  extends SymExpr
  trait StaticSymExpr extends SymExpr

  case class Id(scope: Scope, name: String) {
    override def toString: String = scope.toScopedString(name)

    def toTPId: TPId = new TPId(this)
  }

  sealed trait Scope {

    def +(nestedScope: String): InnerScope = InnerScope(this, nestedScope)

    def makeNewId(): Id                      = Id(this, model.defaultId())
    def toScopedString(name: String): String = s"$this.$name"
  }
  object RootScope extends Scope {
    override def toString: String = "root"
  }
  case class InnerScope(parent: Scope, name: String) extends Scope {

    override def toString: String = parent match {
      case RootScope => name
      case nonScope  => s"$nonScope.$name"
    }
  }

  sealed trait Declaration[T] {
    def id: Id
  }

  case class Type(id: Id, parent: Option[Type]) {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    def overlaps(typ: Type): Boolean = this.isSubtypeOf(typ) || typ.isSubtypeOf(this)

    def asScope: Scope = id.scope + id.name

    override def toString: String = id.toString
  }
  case class TypeDeclaration(typ: Type) extends Declaration[Type] with InModuleBlock with InCore {
    override def id: Id = typ.id
    override def toString: String = s"type $id" + {
      if (typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }

  sealed trait Var extends StaticSymExpr {
    def id: Id
    def typ: Type
  }
  object Var {
    def unapply(v: Var) = Option(v.id, v.typ)
  }

  sealed trait VarDeclaration[V <: Var] extends Declaration[Var] {
    def variable: Var
    def id: Id = variable.id
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class LocalVarDeclaration(variable: LocalVar) extends VarDeclaration[LocalVar] with Statement {
    override def toString: String = s"constant ${variable.typ} ${variable.id}"
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class InstanceDeclaration(instance: Instance) extends VarDeclaration[Instance] with InModuleBlock {
    override def variable: Instance = instance
    override def toString: String   = s"instance ${instance.typ} ${instance.id}"
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] with Statement {
    override def variable: Arg    = arg
    override def toString: String = s"${arg.typ.id} ${arg.id}"
  }

  sealed trait FunctionTemplate {
    def id: Id
    def typ: Type
    def params: Seq[Arg]
  }

  case class FluentTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {
    override def toString: String = id.toString
  }

  case class ConstantTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {
    override def toString: String = id.toString
  }

  case class FunctionDeclaration(func: FunctionTemplate)
      extends Declaration[FunctionTemplate]
      with InModuleBlock
      with InCore {
    override def id: Id = func.id
    override def toString: String = {
      val paramsString = "(" + func.params.map(p => s"${p.typ} ${p.id.name}").mkString(", ") + ")"
      func match {
        case _: FluentTemplate   => s"fluent ${func.typ} ${func.id}$paramsString"
        case _: ConstantTemplate => s"constant ${func.typ} ${func.id}$paramsString"
      }
    }
  }

  private type Param = Var

  sealed trait Function {
    def template: FunctionTemplate
    def params: Seq[Var]
  }

  case class Constant(override val template: ConstantTemplate, override val params: Seq[Var])
      extends full.Constant(template, params)
      with Function {
    override def toString: String = super.toString
  }
  class BoundConstant(override val template: ConstantTemplate, override val params: Seq[Instance])
      extends Constant(template, params)

  case class Fluent(override val template: FluentTemplate, override val params: Seq[Param])
      extends full.Fluent(template, params)
      with Function {

    override def toString = s"$template(${params.mkString(", ")})"
  }

  sealed trait StaticAssertion extends full.StaticAssertion with Statement
  case class StaticEqualAssertion(override val left: Var, override val right: Var)
      extends full.StaticEqualAssertion(left, right)
      with StaticAssertion {
    override def toString: String = super.toString
  }
  case class StaticDifferentAssertion(override val left: Var, override val right: Var)
      extends full.StaticDifferentAssertion(left, right)
      with StaticAssertion {
    override def toString: String = super.toString
  }
  case class StaticAssignmentAssertion(override val left: BoundConstant, override val right: Instance)
      extends full.StaticAssignmentAssertion(left, right)
      with StaticAssertion {
    override def toString: String = super.toString
  }
  case class BindAssertion(constant: Constant, variable: LocalVar) extends StaticAssertion {
    override def toString: String = s"$constant == $variable"
  }

  sealed trait TimedAssertion extends Statement {
    def start: TPRef
    def end: TPRef
    def fluent: Fluent
  }

  /** Denotes an assertion that requires causal support */
  sealed trait RequiresSupport { self: TimedAssertion =>
  }

  /** Denotes an assertion that changes a fluent */
  sealed trait ProvidesChange { self: TimedAssertion =>
    def valueAfterChange: Var
  }

  case class TimedEqualAssertion(start: TPRef, end: TPRef, fluent: Fluent, value: Var)
      extends TimedAssertion
      with RequiresSupport {
    override def toString: String = s"[$start, $end] $fluent == $value"
  }
  case class TimedAssignmentAssertion(start: TPRef, end: TPRef, fluent: Fluent, value: Var)
      extends TimedAssertion
      with ProvidesChange {
    override def valueAfterChange: Var = value
    override def toString: String      = s"[$start,$end] $fluent := $value"
  }
  case class TimedTransitionAssertion(start: TPRef, end: TPRef, fluent: Fluent, from: Var, to: Var)
      extends TimedAssertion
      with RequiresSupport
      with ProvidesChange {
    override def valueAfterChange: Var = to
    override def toString: String      = s"[$start, $end] $fluent == $from :-> $to"
  }

  /*** Time ***/
  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: Int): TBefore       = to <= from + dur
    def <(dur: Int): TBefore        = to < from + dur
    def >=(dur: Int): TBefore       = to >= from + dur
    def >(dur: Int): TBefore        = to > from + dur
    def +(time: Int): Delay         = Delay(from, to + time)
    def -(time: Int): Delay         = Delay(from, to - time)
    def ===(dur: Int): Seq[TBefore] = Seq(this <= dur, this >= dur)
  }

  case class TPId(id: Id) {
    override def toString: String = id.toString
  }
  object TPId {
    implicit def asId(tpId: TPId): Id = tpId.id
  }

  /** A timepoint, declared when appearing in the root of a context.*/
  case class TPRef(id: TPId, delay: Int = 0) {

    override def toString: String =
      id.toString + (delay match {
        case 0          => ""
        case d if d > 0 => s"+$d"
        case d if d < 0 => s"-${-d}"
      })

    def +(delay: Int) = TPRef(id, this.delay + delay)
    def -(delay: Int) = TPRef(id, this.delay - delay)

    def <=(other: TPRef): TBefore       = TBefore(this, other)
    def <(other: TPRef): TBefore        = TBefore(this, other - 1)
    def >=(other: TPRef): TBefore       = other <= this
    def >(other: TPRef): TBefore        = other < this
    def ===(other: TPRef): Seq[TBefore] = Seq(this <= other, this >= other)

    def -(other: TPRef) = Delay(other, this)
  }
  sealed trait SimpleTPRefWitness {
    self: TPRef =>
    require(delay == 0)
  }
  type SimpleTPRef = TPRef with SimpleTPRefWitness
  object SimpleTPRef {
    def apply(id: TPId): SimpleTPRef = new TPRef(id) with SimpleTPRefWitness
  }

  case class TimepointDeclaration(tp: TPRef) extends Declaration[TPRef] with Statement {
    require(tp.delay == 0, "Cannot declare a relative timepoint.")
    override def id: Id           = tp.id
    override def toString: String = s"timepoint $id"
  }

  case class Interval(start: TPRef, end: TPRef) {
    override def toString: String = s"[$start, $end]"
  }

  case class TBefore(from: TPRef, to: TPRef) extends Statement {
    override def toString: String = s"$from <= $to"
  }

  case class ActionTemplate(scope: InnerScope, content: Seq[Statement]) extends InModuleBlock {
    def name: String        = scope.name
    lazy val args: Seq[Arg] = content.collect { case ArgDeclaration(a) => a }

    override def toString: String =
      s"action $name(${args.mkString(", ")}):" + "\n  " + content.mkString("\n  ")

    /** Builds a new action instance with the given name*/
    def instance(instanceName: String): Action = {
      val instanceScope: InnerScope = scope.parent + instanceName

      val trans: InnerScope => InnerScope = (x: InnerScope) => {
        if (x == scope)
          instanceScope
        else
          x
      }

      val instanceContent = landscaper.rewrite(trans, content)
      Action(instanceScope, instanceContent, this)
    }
  }

  /** Instance of an action template */
  case class Action(scope: Scope, content: Seq[Statement], template: ActionTemplate)

}
