package copla.lang

import copla.lang.parsing.anml.Parser
import copla.util.exception._

import scala.collection.mutable

package object model {

  val reservedPrefix       = "__"
  private[this] var nextID = 0
  def defaultId(): String  = reservedPrefix + { nextID += 1; nextID - 1 }

  trait Block
  trait InModuleBlock extends Block
  trait InActionBlock extends Block
  trait Statement  extends InModuleBlock with InActionBlock

  /** A block wrapping other blocks pertaining to the same scope. */
  trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  trait SymExpr extends Block {
    def typ: Type
  }
  trait TimedSymExpr  extends SymExpr
  trait StaticSymExpr extends SymExpr

  abstract class TimedAssertion(parent: Option[Ctx], name: String) extends Ctx with InModuleBlock {
    override val store: BlockStore = new BlockStore() +
      TimepointDeclaration(TPRef(this.id("start"))) +
      TimepointDeclaration(TPRef(this.id("end")))
  }
  case class TimedEqualAssertion(left: TimedSymExpr,
                                 right: StaticSymExpr,
                                 parent: Option[Ctx],
                                 name: String)
      extends TimedAssertion(parent, name) {
    override def toString: String =
      if (name.startsWith(reservedPrefix)) s"$left == $right"
      else s"$name: $left == $right"
  }

  case class TimedTransitionAssertion(fluent: TimedSymExpr,
                                      from: StaticSymExpr,
                                      to: StaticSymExpr,
                                      parent: Option[Ctx],
                                      name: String)
      extends TimedAssertion(parent, name) {
    override def toString: String =
      if (name.startsWith(reservedPrefix)) s"$fluent == $from :-> $to"
      else s"$name: $fluent == $from :-> $to"
  }

  case class TimedAssignmentAssertion(fluent: TimedSymExpr,
                                      to: StaticSymExpr,
                                      parent: Option[Ctx],
                                      name: String)
      extends TimedAssertion(parent, name) {
    override def toString: String =
      if (name.startsWith(reservedPrefix)) s"$fluent := $to"
      else s"$name: $fluent := $to"
  }

  case class TemporallyQualifiedAssertion(interval: Interval, assertion: TimedAssertion)
      extends InModuleBlock
      with InActionBlock
      with Wrapper {

    override def wrapped  = Seq(assertion)
    override def toString = s"$interval $assertion"
  }

  trait StaticAssertion extends InModuleBlock with InActionBlock
  case class StaticEqualAssertion(left: StaticSymExpr, right: StaticSymExpr)
      extends StaticAssertion {
    override def toString: String = s"$left == $right"
  }
  case class StaticDifferentAssertion(left: StaticSymExpr, right: StaticSymExpr)
      extends StaticAssertion {
    override def toString: String = s"$left != $right"
  }
  case class StaticAssignmentAssertion(left: StaticSymExpr, right: StaticSymExpr)
      extends StaticAssertion {
    override def toString: String = s"$left := $right"
  }

  trait Declaration[T] {
    def id: Id
  }

  case class Type(id: Id, parent: Option[Type]) {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    def overlaps(typ: Type): Boolean = this.isSubtypeOf(typ) || typ.isSubtypeOf(this)

    def asScope: Scope = id.scope + id.name

    override def toString: String = id.toString
  }
  case class TypeDeclaration(typ: Type) extends Declaration[Type] with InModuleBlock {
    override def id: Id = typ.id
    override def toString: String = s"type $id" + {
      if (typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }

  sealed trait FunctionTemplate {
    def id: Id
    def typ: Type
    def params: Seq[Arg]
  }

  case class FluentTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {

    override def toString: String = id.toString
  }
  case class FunctionDeclaration(func: FunctionTemplate)
      extends Declaration[FunctionTemplate]
      with InModuleBlock {
    override def id: Id = func.id
    override def toString: String = {
      val paramsString = "(" + func.params.map(p => s"${p.typ} ${p.id.name}").mkString(", ") + ")"
      func match {
        case _: FluentTemplate   => s"fluent ${func.typ} ${func.id}$paramsString"
        case _: ConstantTemplate => s"constant ${func.typ} ${func.id}$paramsString"
      }
    }
  }

  case class Fluent(template: FluentTemplate, params: Seq[StaticSymExpr]) extends TimedSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString = s"$template(${params.mkString(", ")})"
  }

  case class ConstantTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {

    override def toString: String = id.toString
  }

  case class Constant(template: ConstantTemplate, params: Seq[StaticSymExpr])
      extends StaticSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString: String = s"$template(${params.mkString(", ")})"
  }

  trait Var extends StaticSymExpr {
    def id: Id
    def typ: Type
  }
  object Var {
    def unapply(v: Var) = Option(v.id, v.typ)
  }
  trait VarDeclaration[V <: Var] extends Declaration[Var] {
    def variable: Var
    def id: Id = variable.id
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var
  case class LocalVarDeclaration(variable: LocalVar)
      extends VarDeclaration[LocalVar]
      with InModuleBlock {
    override def toString: String = s"constant ${variable.typ} ${variable.id}"
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class InstanceDeclaration(instance: Instance)
      extends VarDeclaration[Instance]
      with InModuleBlock {
    override def variable: Instance = instance
    override def toString: String   = s"instance ${instance.typ} ${instance.id}"
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] with InActionBlock {
    override def variable: Arg    = arg
    override def toString: String = s"${arg.typ.id} ${arg.id}"
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

  /** A timepoint, declared when appearing in the root of a context.*/
  case class TPRef(id: Id, delay: Int = 0) {

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
  case class TimepointDeclaration(tp: TPRef)
      extends Declaration[TPRef]
      with InModuleBlock
      with InActionBlock {
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

  case class Id(scope: Scope, name: String) {
    override def toString: String = scope.toScopedString(name)
  }

  case class Scope(path: Seq[String]) {

    def +(nestedScope: String): Scope = Scope(path :+ nestedScope)

    override def toString: String            = path.mkString(".")
    def toScopedString(name: String): String = (path :+ name).mkString(".")
  }

  class ActionTemplate(override val name: String,
                       val containingModel: Model,
                       override val store: BlockStore)
      extends Ctx
      with InModuleBlock {
    override def parent: Option[Ctx] = Some(containingModel)

    def +(block: InActionBlock): ActionTemplate =
      new ActionTemplate(name, containingModel, store + block)
    def ++(newBlocks: Seq[InActionBlock]): ActionTemplate = newBlocks.headOption match {
      case Some(first) => (this + first) ++ newBlocks.tail
      case None        => this
    }

    override def toString: String =
      s"action $name(${store.blocks.collect { case ArgDeclaration(Arg(id, typ)) => s"$typ ${id.name}" }.mkString(", ")}) {\n" +
        store.blocks.map("    " + _.toString).mkString("\n") +
        "  };"
  }

  class BlockStore private(val blocks: Vector[Block], val declarations: Map[Id, Declaration[_]]) {

    def this() = this(Vector(), Map())

    def +(b: Block): BlockStore = {
      val newBlocks = blocks :+ b
      val toProcess = (b match {
        case wrapper: Wrapper =>
          wrapper +: wrapper.wrapped
        case x => Seq(x)
      }).flatMap {
        case ctx: Ctx => ctx.store.blocks :+ ctx
        case x        => Seq(x)
      }
      val newDeclarations = declarations ++ toProcess.collect {
        case x: Declaration[_] => (x.id, x)
      }

      new BlockStore(newBlocks, newDeclarations)
    }
  }

  trait Ctx {

    final val scope: Scope = parent.map(_.scope + name).getOrElse(Scope(Seq()))
    assert(scope != null)

    def id(name: String): Id = Id(scope, name)

    def parent: Option[Ctx]
    def name: String
    def root: Ctx = parent match {
      case Some(p) => p.root
      case None    => this
    }
    def store: BlockStore

    def findDeclaration(localID: String): Option[Declaration[_]] = {
      (localID.split("\\.").toList match {
        case single :: Nil =>
          store.declarations.get(id(single))
        case subScopeName :: name :: Nil =>
          store.declarations
            .get(Id(scope + subScopeName, name))
        case Nil =>
          sys.error("Invalid name: " + localID)
        case _ =>
          sys.error(s"No support for multiple nested declarations: $localID")
      }).orElse(parent.flatMap(_.findDeclaration(localID)))
    }

    def findVariable(name: String): Option[Var] =
      findDeclaration(name: String).flatMap {
        case decl: VarDeclaration[_] => Some(decl.variable)
        case _                       => None
      }

    def findTimepoint(name: String): Option[TPRef] =
      findDeclaration(name).flatMap {
        case TimepointDeclaration(tp) => Some(tp)
        case _                        => None
      }

    def findType(name: String): Option[Type] =
      findDeclaration(name).flatMap {
        case TypeDeclaration(typ) => Some(typ)
        case _                    => None
      }

    def findFunction(name: String): Option[FunctionTemplate] =
      findDeclaration(name).flatMap {
        case FunctionDeclaration(t) => Some(t)
        case _                      => None
      }

    def findFluent(name: String): Option[FluentTemplate] =
      findFunction(name).flatMap {
        case t: FluentTemplate => Some(t)
        case _                 => None
      }

    def findConstant(name: String): Option[ConstantTemplate] =
      findFunction(name).flatMap {
        case t: ConstantTemplate => Some(t)
        case _                   => None
      }

  }

  case class Model(store: BlockStore = new BlockStore()) extends Ctx {
    override def parent = None
    override def name   = "_module_"

    def +(block: InModuleBlock): Option[Model] = {
      Some(Model(store + block))
    }

    def ++(blocks: Seq[InModuleBlock]): Option[Model] = {
      blocks.foldLeft(Option(this))((m, block) => m.flatMap(_ + block))
    }

    override def toString: String =
      "module:\n" +
        store.blocks
          .filter(!Parser.baseAnmlModel.store.blocks.contains(_))
          .map("  " + _)
          .mkString("\n")
  }
}
