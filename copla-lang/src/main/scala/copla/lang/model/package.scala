package copla.lang

import copla.lang.parsing.anml.Parser
import copla.util.exception._

import scala.collection.mutable

package object model {

  val reservedPrefix       = "__"
  private[this] var nextID = 0
  def defaultId(): String  = reservedPrefix + { nextID += 1; nextID - 1 }

  trait Elem
  trait ModuleElem extends Elem
  trait ActionElem extends Elem
  trait Statement  extends ModuleElem with ActionElem

  /** An elem wrapping otehr elems pertaining to the same scope. */
  trait Wrapper extends Elem {
    def wrapped: Seq[Elem]
  }

  trait SymExpr extends Elem {
    def typ: Type
  }
  trait TimedSymExpr  extends SymExpr
  trait StaticSymExpr extends SymExpr

  abstract class TimedAssertion(parent: Option[Ctx], name: String) extends Ctx with ModuleElem {
    override val elems =
      Seq(TimepointDeclaration(TPRef(this.id("start"))),
          TimepointDeclaration(TPRef(this.id("end"))))
  }
  case class TimedEqualAssertion(left: TimedSymExpr,
                                 right: StaticSymExpr,
                                 parent: Option[Ctx],
                                 name: String)
      extends TimedAssertion(parent, name) {
    override def toString =
      if (name.startsWith(reservedPrefix)) s"$left == $right"
      else s"$name: $left == $right"
  }
  case class TemporallyQualifiedAssertion(interval: Interval, assertion: TimedAssertion)
      extends ModuleElem
      with ActionElem
      with Wrapper {

    override def wrapped  = Seq(assertion)
    override def toString = s"$interval $assertion"
  }

  trait Declaration[T] {
    def id: Id
  }

  case class Type(id: Id, parent: Option[Type]) {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    def asScope: Scope = id.scope + id.name

    override def toString = id.toString
  }
  case class TypeDeclaration(typ: Type) extends Declaration[Type] with ModuleElem {
    override def id = typ.id
    override def toString = s"type $id" + {
      if (typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }

  sealed trait FunctionTemplate {
    def id: Id
    def typ: Type
    def params: Seq[Arg]
  }

  case class FluentTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {

    override def toString = id.toString
  }
  case class FunctionDeclaration(func: FunctionTemplate)
      extends Declaration[FunctionTemplate]
      with ModuleElem {
    override def id = func.id
    override def toString = func match {
      case _: FluentTemplate   => s"fluent ${func.typ} ${func.id}(${func.params.mkString(", ")})"
      case _: ConstantTemplate => s"constant ${func.typ} ${func.id}(${func.params.mkString(", ")})"
    }
  }

  case class Fluent(template: FluentTemplate, params: Seq[StaticSymExpr]) extends TimedSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ = template.typ

    override def toString = s"$template(${params.mkString(", ")})"
  }

  case class ConstantTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {

    override def toString = id.toString
  }

  case class Constant(template: ConstantTemplate, params: Seq[StaticSymExpr])
      extends StaticSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ = template.typ

    override def toString = s"$template(${params.mkString(", ")})"
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
      with ModuleElem {
    override def toString = s"constant ${variable.typ} ${variable.id}"
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Var {
    override def toString = id.toString
  }
  case class InstanceDeclaration(instance: Instance)
      extends VarDeclaration[Instance]
      with ModuleElem {
    override def variable = instance
    override def toString = s"instance ${instance.typ} ${instance.id}"
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString = s"${typ.id} $id"
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] with ActionElem {
    override def variable = arg
    override def toString = arg.toString
  }

  /*** Time ***/
  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: Int)  = to <= from + dur
    def <(dur: Int)   = to < from + dur
    def >=(dur: Int)  = to >= from + dur
    def >(dur: Int)   = to > from + dur
    def +(time: Int)  = Delay(from, to + time)
    def -(time: Int)  = Delay(from, to - time)
    def ===(dur: Int) = Seq(this <= dur, this >= dur)
  }

  /** A timepoint, declared when appearing in the root of a context.*/
  case class TPRef(id: Id, delay: Int = 0) {

    override def toString =
      id.toString + (delay match {
        case 0          => ""
        case d if d > 0 => s"+$d"
        case d if d < 0 => s"-${-d}"
      })

    def +(delay: Int) = TPRef(id, this.delay + delay)
    def -(delay: Int) = TPRef(id, this.delay - delay)

    def <=(other: TPRef)  = TBefore(this, other)
    def <(other: TPRef)   = TBefore(this, other - 1)
    def >=(other: TPRef)  = other <= this
    def >(other: TPRef)   = other < this
    def ===(other: TPRef) = Seq(this <= other, this >= other)

    def -(other: TPRef) = Delay(other, this)
  }
  case class TimepointDeclaration(tp: TPRef) extends Declaration[TPRef] with ModuleElem with ActionElem {
    require(tp.delay == 0, "Cannot declare a relative timepoint.")
    override def id       = tp.id
    override def toString = s"timepoint $id"
  }

  case class Interval(start: TPRef, end: TPRef) {
    override def toString = s"[$start, $end]"
  }

  case class TBefore(from: TPRef, to: TPRef) extends Statement {
    override def toString: String = s"$from <= $to"
  }

  case class Id(scope: Scope, name: String) {
    override def toString = scope.toInScopeString(name)
  }

  case class Scope(path: Seq[String]) {

    def +(nestedScope: String): Scope = Scope(path :+ nestedScope)

    override def toString             = path.mkString(".")
    def toInScopeString(name: String) = (path :+ name).mkString(".")
  }

  class ActionTemplate(override val name: String, val containingModel: Model, val elems: Seq[ActionElem])
    extends Ctx
      with ModuleElem {
    override def parent: Option[Ctx] = Some(containingModel)

    def +(elem: ActionElem): ActionTemplate = new ActionTemplate(name, containingModel, elems :+ elem)
    def ++(newElems: Seq[ActionElem]): ActionTemplate = newElems.headOption match {
      case Some(first) => (this + first) ++ newElems.tail
      case None        => this
    }

    override def toString: String =
      s"action $name(${elems.collect { case ArgDeclaration(Arg(id, typ)) => s"$typ ${id.name}" }.mkString(", ")}) {\n" +
        elems.map("    " + _.toString).mkString("\n") +
        "  };"
  }

  trait Ctx {
    val elems: Seq[Elem]
    def parent: Option[Ctx]
    def name: String
    def root: Ctx = parent match {
      case Some(p) => p.root
      case None    => this
    }

    final val scope: Scope = parent.map(_.scope + name).getOrElse(Scope(Seq()))
    assert(scope != null)

    def allElems: Seq[Elem] = elems.flatMap {
      case wrapper: Wrapper => wrapper +: wrapper.wrapped
      case any: Any         => Seq(any)
    }

    private[this] lazy val declarations: Map[Id, Declaration[_]] =
      allElems.collect {
        case x: Declaration[_] => (x.id, x)
      }.toMap

    private[this] lazy val subContexts: Map[String, Ctx] =
      allElems.collect {
        case x: Ctx => (x.name, x)
      }.toMap

    def id(name: String): Id = Id(scope, name)

    def findDeclaration(localID: String): Option[Declaration[_]] = {
      (localID.split("\\.").toList match {
        case single :: Nil =>
          declarations.get(id(single))
        case subScopeName :: name :: Nil =>
          declarations
            .get(Id(scope + subScopeName, name))
            .orElse(
              subContexts.get(subScopeName).flatMap(_.findDeclaration(name))
            )
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

  case class Model(elems: Seq[ModuleElem] = mutable.Buffer()) extends Ctx {
    override def parent = None
    override def name   = "_module_"

    def +(elem: ModuleElem): Option[Model] = {
      Some(Model(elem +: elems))
    }

    def ++(elems: Seq[ModuleElem]): Option[Model] = {
      elems.foldLeft(Option(this))((m, elem) => m.flatMap(_ + elem))
    }

    override def toString =
      "module:\n" +
        elems
          .filter(!Parser.baseAnmlModel.elems.contains(_))
          .map("  " + _)
          .reverse
          .mkString("\n")
  }
}
