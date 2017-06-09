package copla

import copla.lang.template

import scala.collection.mutable
import copla.lang.template._
import Lang._

import scala.languageFeature.implicitConversions

object Lang {
  sealed trait Literal
  case class LInt(value: Int) extends Literal
  implicit def int2lint(v: Int): LInt = LInt(v)

  case class Instance(typ: Type, name: Symbol)(implicit val module: Module) {
    module.declaredInstances += this

    val tVariable                 = TVariable(name, typ) // creation is needed
    override def toString: String = name.toString()
  }
  def instance(typ: Type, name: Symbol)(implicit module: Module) =
    Instance(typ, name).tVariable.ref

  def instances(typ: Type, names: Symbol*)(implicit module: Module): List[VRef] = {
    names.map(Instance(typ, _).tVariable.ref).toList // create all instances
  }

  class Module(val name: Symbol) extends Context {
    implicit val ctx           = this
    override def isTemplate    = false
    override def parentContext = None

    val declaredInstances = mutable.Buffer[Instance]()
    val types             = mutable.Buffer[Type]()
    val stateVariables    = mutable.Buffer[StateVariableTemplate]()

    val int      = Type('int)
    val start    = timepoint('start)
    val end      = timepoint('end)
    val makespan = Delay(start, end) + 1

    val elems = mutable.ArrayBuffer[Elem]()

    def add(elems: Elem*) { this.elems ++= elems }
    def core(elems: Elem*) { this.elems ++= elems }
  }

  trait Elem

  case class Type(name: Symbol)(implicit val module: Module) {
    module.types += this
    def apply(argumentName: Symbol): Arg = Arg(this, argumentName)

    var superType: Option[Type] = None
    val subTypes                = mutable.ArrayBuffer[Type]()

    def <(parent: Type): Type = {
      assert(
        superType.isEmpty,
        s"Type '$name' already has a super type (${superType.map(_.toString).getOrElse("???")}) when trying to make it inherit $parent")
      superType = Some(parent)
      parent.subTypes += this
      this
    }

    def isSubtypeOf(typ: Type): Boolean =
      this == typ || superType.exists(t => t == typ || t.isSubtypeOf(typ))

    def instances: Seq[Instance] = module.declaredInstances.filter(_.typ.isSubtypeOf(this))
  }
  implicit def symbol2type(s: Symbol)(implicit ctx: Context) = {
    ctx.module.types.find(_.name == s) match {
      case Some(typ) => typ
      case None      => sys.error(s"Unable to find a type named $s")
    }
  }

  case class Arg(typ: Type, name: Symbol)

  case class StateVariableTemplate(name: Symbol, typ: Type, params: Seq[Arg])(
      implicit ctx: Context)
      extends Elem {
    require(ctx == ctx.module, "State variables can only be declared at top level")

    def apply(variables: VRef*)(implicit ctx: Context): StateVariable = {
      require(params.size == variables.size,
              s"Wrong number of arguments for state variable, $name")
      StateVariable(this, variables)
    }
  }

  case class StateVariable(template: StateVariableTemplate, params: Seq[VRef])(
      implicit ctx: Context) {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, ref) =>
        require(ref.variable.typ.isSubtypeOf(tpl.typ), s"$ref is not of type ${tpl.typ}")
    }

    def ===(variable: VRef): InnerEqualAssertion =
      InnerEqualAssertion(this, variable)
    def ===(transition: Transition): InnerTransitionAssertion =
      InnerTransitionAssertion(this, transition.vStart, transition.vEnd)

    override def toString = s"${template.name}(${params.mkString(", ")})"
  }

  def sv(name: Symbol, typ: Type, params: Arg*)(implicit module: Module): StateVariableTemplate = {
    val sv = StateVariableTemplate(name, typ, params)
    module.add(sv)
    sv
  }

  case class Variable(id: Symbol, typ: Type) {
    override def toString = id.toString()
  }

  case class Interval(start: TPRef, end: TPRef) {
    def apply(assertion: InnerAssertion): TAssertion =
      TAssertion(this, assertion)
    override def toString =
      if (start == end) s"[$start]" else s"[$start, $end]"
  }
  def at(tp: TPRef): Interval                  = Interval(tp, tp)
  def over(start: TPRef, end: TPRef): Interval = Interval(start, end)

  case class TAssertion(interval: Interval, inner: InnerAssertion) extends Elem {
    override def toString = s"$interval $inner"
  }
  trait InnerAssertion
  case class InnerEqualAssertion(sv: StateVariable, value: VRef)(implicit ctx: Context)
      extends InnerAssertion {
    val start = timepoint('start)
    val end   = timepoint('end)
    require(value.variable.typ.isSubtypeOf(sv.template.typ),
            s"Variable $value is not of the expected type ${sv.template.typ}")
    override def toString = s"$sv === $value"
  }

  case class InnerTransitionAssertion(sv: StateVariable, startValue: VRef, endValue: VRef)(
      implicit ctx: Context)
      extends InnerAssertion {
    List(startValue, endValue).foreach(
      v =>
        require(v.variable.typ.isSubtypeOf(sv.template.typ),
                s"Variable $v is not of the expected type ${sv.template.typ}"))

    val start = timepoint('start)
    val end   = timepoint('end)

    override def toString = s"$sv === $startValue -> $endValue"
  }

  case class TVariable(id: Symbol, typ: Type)(implicit ctx: Context) {
    val global: Option[Variable] =
      if (ctx.isTemplate) None
      else Some(Variable(id, typ))
    ctx.localVariables += this

    def ref: VRef = new VRef(id)
  }

  trait Context {
    def isTemplate: Boolean
    def parentContext: Option[Context]
    val module: Module = this match {
      case m: Module => m
      case _ =>
        parentContext match {
          case Some(parent) => parent.module
          case None         => sys.error("This context is has no module as parent")
        }
    }

    val localVariables  = mutable.ArrayBuffer[TVariable]()
    val localTimepoints = mutable.ArrayBuffer[TTimepoint]()

    def findVariable(id: Symbol): Option[TVariable] = {
      localVariables.find(_.id == id) match {
        case Some(v) => Some(v)
        case None =>
          parentContext.flatMap(p => p.findVariable(id))
      }
    }
    def findTimepoint(id: Symbol): Option[TTimepoint] = {
      localTimepoints.find(_.id == id) match {
        case Some(v) => Some(v)
        case None =>
          parentContext.flatMap(p => p.findTimepoint(id))
      }
    }
  }

  implicit class VRef(val id: Symbol)(implicit ctx: Context) {
    val variable = ctx.findVariable(id) match {
      case Some(v) => v
      case None    => sys.error(s"Could not find a variable with id: ${id.name}")
    }
    def ->(other: VRef): Transition = Transition(this, other)

    override def toString = id.toString()
  }
  case class Transition(vStart: VRef, vEnd: VRef)
}

object Main extends App {

  val m = new Module('test) {
    Type('Location)
    Type('NavLocation) < 'Location
    Type('Robot)

    instances('Robot, 'r1, 'r2, 'r3)
    instances('NavLocation, 'l1, 'l2, 'l3)

    val loc = sv('loc, 'Location, 'Robot ('r))

    core(
      end <= 10,
      start <= 20,
      makespan <= 10,
      at(start)(loc('r1) === 'l1 -> 'l2),
      over(start, end)(loc('r1) === 'l3)
    )
    val go = new template.Action('Go, Nil) {
      core(
        duration <= 10
      )
    }
  }

  println(m.elems.mkString("\n"))
  println(m.declaredInstances)
  println(m.types)
}
