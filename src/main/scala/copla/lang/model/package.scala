package copla.lang

import copla.lang.parsing.anml.Parser
import exception._

import scala.collection.mutable

package object model {

  val reservedPrefix = "__"
  private[this] var nextID = 0

  def defaultId(): String = reservedPrefix + {
    nextID += 1; nextID - 1
  }

  trait Elem

  trait ModuleElem extends Elem

  trait Statement extends ModuleElem

  /** An elem wrapping otehr elems pertaining to the same scope. */
  trait Wrapper extends Elem {
    def wrapped: Seq[Elem]
  }

  trait SymExpr extends Elem {
    def typ: Type
  }

  trait TimedSymExpr extends SymExpr

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
      if(name.startsWith(reservedPrefix)) s"$left == $right"
      else s"$name: $left == $right"
  }

  case class TemporallyQualifiedAssertion(interval: Interval, assertion: TimedAssertion)
    extends ModuleElem
      with Wrapper {

    override def wrapped = Seq(assertion)

    override def toString = s"$interval $assertion"
  }

  trait Declaration[T] {
    def id: Id
  }

  class FutureType(name: String) {
    def map(f: PartialFunction[FutureType, FutureType]) =
      f.applyOrElse(this, (x: FutureType) => x)
  }

  case class Type(id: Id, parent: Option[Type]) extends FutureType(id.name) with Final {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    override def toString = id.toString
  }

  case class TypeDeclaration(typ: Type) extends Declaration[Type] with ModuleElem {
    override def id = typ.id

    override def toString = s"type $id" + {
      if(typ.parent.isDefined) " < " + typ.parent.get else ""
    }
  }

  trait Final

  class FutureArg(id: Id, typ: FutureType)
  class FutureFluentTemplate(id: Id, typ: FutureType, params: Seq[FutureArg]) extends Elem

  case class FluentTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FutureFluentTemplate(id, typ, params) {
    def apply(variables: Var*): Fluent = {
      require(params.size == variables.size, s"Wrong number of arguments for state variable, $id")
      Fluent(this, variables)
    }

    override def toString = id.toString
  }
  case class FluentDeclaration(fluent: FluentTemplate)
      extends Declaration[FluentTemplate]
      with ModuleElem {
    override def id       = fluent.id
    override def toString = s"fluent ${fluent.typ} ${fluent.id}(${fluent.params.mkString(", ")})"
  }

  case class Fluent(template: FluentTemplate, params: Seq[Var]) extends TimedSymExpr {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

    override def typ = template.typ

//    def ===(transition: Transition): InnerTransitionAssertion =
//      InnerTransitionAssertion(this, transition.vStart, transition.vEnd)

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
  case class Arg(id: Id, typ: Type) extends FutureArg(id, typ) with Var{
    override def toString = s"${typ.id} $id"
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] {
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
  case class TimepointDeclaration(tp: TPRef) extends Declaration[TPRef] with ModuleElem {
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

    private[this] lazy val declarations: Map[String, Declaration[_]] =
      allElems.collect {
        case x: Declaration[_] => {
          assert(x.id.scope == this.scope)
          (x.id.name, x)
        }
      }.toMap

    private[this] lazy val subContexts: Map[String, Ctx] =
      allElems.collect {
        case x: Ctx => (x.name, x)
      }.toMap

    def id(name: String): Id = Id(scope, name)

    def findDeclaration(name: String): Option[Declaration[_]] = {
      (name.split("\\.").toList match {
        case single :: Nil =>
          declarations.get(single)
        case head :: tail =>
          subContexts.get(head).flatMap(sc => sc.findDeclaration(tail.mkString(".")))
        case Nil =>
          sys.error("Invalid name: " + name)
      }).orElse(parent.flatMap(_.findDeclaration(name)))
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

    def findFluent(name: String): Option[FluentTemplate] =
      findDeclaration(name).flatMap {
        case FluentDeclaration(t) => Some(t)
        case _                    => None
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
