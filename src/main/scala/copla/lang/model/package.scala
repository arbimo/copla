package copla.lang

import copla.lang.parsing.anml.Parser
import exception._

package object model {

  trait Elem
  trait ModuleElem extends Elem
  trait Statement extends ModuleElem
  trait TimedSymExpr extends Elem

  trait Declaration[T] {
    def id: Id
  }

  case class Type(id: Id, parent: Option[Type]) extends ModuleElem {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    override def toString = id.toString
  }



  case class StateVariableTemplate(name: String, typ: Type, params: Seq[Arg]) extends ModuleElem {

    def apply(variables: Var*): StateVariable = {
      require(params.size == variables.size,
              s"Wrong number of arguments for state variable, $name")
      StateVariable(this, variables)
    }

    override def toString = s"$name(${params.mkString(", ")})->${typ.id}"
  }

  case class StateVariable(template: StateVariableTemplate, params: Seq[Var]) extends ModuleElem {
    require(template.params.size == params.size)
    template.params.zip(params).foreach {
      case (tpl, v) =>
        require(v.typ.isSubtypeOf(tpl.typ), s"$v is not of type ${tpl.typ}")
    }

//    def ===(variable: VRef): InnerEqualAssertion =
//      InnerEqualAssertion(this, variable)
//    def ===(transition: Transition): InnerTransitionAssertion =
//      InnerTransitionAssertion(this, transition.vStart, transition.vEnd)

    override def toString = s"${template.name}(${params.mkString(", ")})"
  }

  trait Var extends Elem {
    def id: Id
    def typ: Type
  }
  object Var {
    def unapply(v: Var)= Option(v.id, v.typ)
  }
  trait VarDeclaration[V <: Var] extends Declaration[Var] {
    def variable: V
    def id: Id = variable.id
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var with ModuleElem
  case class LocalVarDeclaration(variable: LocalVar) extends VarDeclaration[LocalVar] {
    override def toString = s"constant ${variable.typ} ${variable.id}"
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Var with ModuleElem
  case class InstanceDeclaration(instance: Instance) extends VarDeclaration[Instance] {
    override def variable = instance
    override def toString = s"instance ${instance.typ} ${instance.id}"
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString = s"${typ.id} $id"
  }
  case class ArgDeclaration(arg: Arg) extends VarDeclaration[Arg] {
    override def variable = arg
    override def toString = arg.toString
  }

  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: Int) = to <= from + dur
    def <(dur: Int)  = to < from + dur
    def >=(dur: Int) = to >= from + dur
    def >(dur: Int)  = to > from + dur
    def +(time: Int) = Delay(from, to + 1)
    def -(time: Int) = Delay(from, to - 1)
  }

  /** A timepoint, declared when appearing in the root of a context.*/
  case class TPRef(id: Id, delay: Int = 0) extends ModuleElem {

    override def toString = id.toString + (delay match {
      case 0 => ""
      case d if d > 0 => s"+$d"
      case d if d < 0 => s"-${-d}"
    })

    def +(delay: Int) = TPRef(id, this.delay + delay)
    def -(delay: Int) = TPRef(id, this.delay - delay)

    def <=(other: TPRef) = TBefore(this, other)
    def <(other: TPRef)  = TBefore(this, other + 1)
    def >=(other: TPRef) = other <= this
    def >(other: TPRef)  = other < this
  }
  case class TimepointDeclaration(tp: TPRef) extends Declaration[TPRef] {
    require(tp.delay == 0, "Cannot declare a relative timepoint.")
    override def id = tp.id
    override def toString = s"timepoint $id"
  }

  case class TBefore(from: TPRef, to: TPRef) extends Elem with Statement {
    override def toString: String = s"$from <= $to"
  }

  case class Id(scope: Scope, name: String) {
    override def toString = scope.toInScopeString(name)
  }

  case class Scope(path: Seq[String]) {

    def +(nestedScope: String) : Scope = Scope(path :+ nestedScope)

    override def toString = path.mkString(".")
    def toInScopeString(name: String) = (path :+ name).mkString(".")
  }

  trait Ctx {
    def elems: Seq[Elem]
    def parent: Option[Ctx]
    def name: String

    val scope: Scope = parent.map(_.scope +name).getOrElse(Scope(Seq(name)))

    def id(name: String): Id = Id(scope, name)

    def findVariable(name: String): Option[Var] =
      elems
        .collect { case x @ Var(Id(_, `name`), _) => x }
        .headOption
        .orElse(parent.flatMap(_.findVariable(name)))

    def findTimepoint(name: String): Option[TPRef] =
      elems
        .collect { case x @ TPRef(Id(_, `name`), _) => x }
        .headOption
        .orElse(parent.flatMap(_.findTimepoint(name)))

    def findType(name: String): Option[Type] =
      elems.collect { case t@Type(Id(_, `name`), _) => t }
      .headOption
      .orElse(parent.flatMap(_.findType(name)))
  }

  case class Model(elems: Seq[ModuleElem] = Seq()) extends Ctx {
    override def parent = None
    override def name = "_module_"
    override val scope = Scope(Seq()) // root scope

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
          .map("  "+_)
          .reverse
          .mkString("\n")
  }
}
