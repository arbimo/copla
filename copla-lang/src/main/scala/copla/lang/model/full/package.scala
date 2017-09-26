package copla.lang.model

import copla.lang.model.core.{InnerScope, RootScope, SimpleTPRef}
import copla.lang.parsing.anml.Parser

package object full {

  trait Block
  trait InModuleBlock extends Block
  trait InActionBlock extends Block
  trait Statement     extends InModuleBlock with InActionBlock

  type Declaration[T]           = core.Declaration[T]
  type Var                      = core.Var
  type VarDeclaration[T <: Var] = core.VarDeclaration[T]
  type Instance                 = core.Instance
  type InstanceDeclaration      = core.InstanceDeclaration
  type LocalVar                 = core.LocalVar
  type LocalVarDeclaration      = core.LocalVarDeclaration
  type Arg                      = core.Arg
  type ArgDeclaration           = core.ArgDeclaration
  type TPRef                    = core.TPRef
  type SimpleTPRef              = core.SimpleTPRef
  type TimepointDeclaration     = core.TimepointDeclaration
  type Interval                 = core.Interval
  type Delay                    = core.Delay
  type TBefore                  = core.TBefore
  type FunctionTemplate         = core.FunctionTemplate
  type FunctionDeclaration      = core.FunctionDeclaration
  type ConstantTemplate         = core.ConstantTemplate
  type FluentTemplate           = core.FluentTemplate
  type Type                     = core.Type
  type TypeDeclaration          = core.TypeDeclaration
  type Id                       = core.Id
  type Scope                    = core.Scope

  type SymExpr       = core.SymExpr
  type TimedSymExpr  = core.TimedSymExpr
  type StaticSymExpr = core.StaticSymExpr

  /** A block wrapping other blocks pertaining to the same scope. */
  sealed trait Wrapper extends Block {
    def wrapped: Seq[Block]
  }

  abstract class TimedAssertion(parent: Option[Ctx], name: String) extends Ctx with Block {

    val start: SimpleTPRef = SimpleTPRef(this.id("start").toTPId)
    val end: SimpleTPRef   = SimpleTPRef(this.id("end").toTPId)

    override val store: BlockStore[Statement] = new BlockStore[Statement]() +
      new TimepointDeclaration(start) +
      new TimepointDeclaration(end)
  }
  case class TimedEqualAssertion(left: TimedSymExpr, right: StaticSymExpr, parent: Option[Ctx], name: String)
      extends TimedAssertion(parent, name) {
    if (name == "__296")
      println(name)
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

  trait TemporalQualifier
  case class Equals(interval: Interval) extends TemporalQualifier {
    override def toString: String = interval.toString
  }
  case class Contains(interval: Interval) extends TemporalQualifier {
    override def toString: String = s"$interval contains"
  }

  case class TemporallyQualifiedAssertion(qualifier: TemporalQualifier, assertion: TimedAssertion)
      extends Statement
      with Wrapper {

    override def wrapped  = Seq(assertion)
    override def toString = s"$qualifier $assertion"
  }

  trait StaticAssertion extends Statement
  class StaticEqualAssertion(val left: StaticSymExpr, val right: StaticSymExpr) extends StaticAssertion {
    override def toString: String = s"$left == $right"
  }
  class StaticDifferentAssertion(val left: StaticSymExpr, val right: StaticSymExpr) extends StaticAssertion {
    override def toString: String = s"$left != $right"
  }
  class StaticAssignmentAssertion(val left: StaticSymExpr, val right: StaticSymExpr) extends StaticAssertion {
    override def toString: String = s"$left := $right"
  }

  class Fluent(val template: FluentTemplate, val params: Seq[StaticSymExpr]) extends TimedSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString = s"$template(${params.mkString(", ")})"
  }

  class Constant(val template: ConstantTemplate, val params: Seq[StaticSymExpr]) extends StaticSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ: Type = template.typ

    override def toString: String = s"$template(${params.mkString(", ")})"
  }

  class ActionTemplate(override val name: String,
                       val containingModel: Model,
                       override val store: BlockStore[InActionBlock] = new BlockStore())
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
      s"action $name(${store.blocks
        .collect { case x: ArgDeclaration => s"${x.arg.typ} ${x.arg.id.name}" }
        .mkString(", ")}) {\n" +
        store.blocks.map("    " + _.toString).mkString("\n") +
        "  };"
  }

  case class Model(store: BlockStore[InModuleBlock] = new BlockStore()) extends Ctx {
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

  class BlockStore[+T <: Block] private (val blocks: Vector[T], val declarations: Map[Id, Declaration[_]]) {

    def this() = this(Vector(), Map())

    def +[B >: T <: Block](b: B): BlockStore[B] = {
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

      new BlockStore[B](newBlocks, newDeclarations)
    }
  }

  trait Ctx {

    final val scope: InnerScope = parent.map(_.scope).getOrElse(RootScope) + name
    assert(scope != null)

    def id(name: String): Id = new Id(scope, name)

    def parent: Option[Ctx]
    def name: String
    def root: Ctx = parent match {
      case Some(p) => p.root
      case None    => this
    }
    def store: BlockStore[Block]

    def findDeclaration(localID: String): Option[Declaration[_]] = {
      (localID.split("\\.").toList match {
        case single :: Nil =>
          store.declarations.get(id(single))
        case subScopeName :: name :: Nil =>
          store.declarations
            .get(new Id(scope + subScopeName, name))
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
        case decl: TimepointDeclaration => Some(decl.tp)
        case _                          => None
      }

    def findType(name: String): Option[Type] =
      findDeclaration(name).flatMap {
        case decl: TypeDeclaration => Some(decl.typ)
        case _                     => None
      }

    def findFunction(name: String): Option[FunctionTemplate] =
      findDeclaration(name).flatMap {
        case decl: FunctionDeclaration => Some(decl.func)
        case _                         => None
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
}
