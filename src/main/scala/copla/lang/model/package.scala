package copla.lang

import exception._

package object model {

  trait Elem
  trait ModuleElem extends Elem
  trait Statement extends ModuleElem
  trait TimedSymExpr extends Elem

  case class Type(name: String, parent: Option[Type]) extends ModuleElem {

    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))
  }



  case class StateVariableTemplate(name: String, typ: Type, params: Seq[Arg]) extends ModuleElem {

    def apply(variables: Var*): StateVariable = {
      require(params.size == variables.size,
              s"Wrong number of arguments for state variable, $name")
      StateVariable(this, variables)
    }

    override def toString = s"$name(${params.mkString(", ")})->${typ.name}"
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

  trait Var {
    def id: Id
    def typ: Type
  }
  object Var {
    def unapply(v: Var)= Option(v.id, v.typ)
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var with ModuleElem

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Var with ModuleElem

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString = s"${typ.name} $id"
  }

  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: Int) = to <= from + dur
    def <(dur: Int)  = to < from + dur
    def >=(dur: Int) = to >= from + dur
    def >(dur: Int)  = to > from + dur
    def +(time: Int) = Delay(from, to + 1)
    def -(time: Int) = Delay(from, to - 1)
  }

  class TPRef(val id: Id, val delay: Int = 0) {

    override def toString = id.toString

    def +(delay: Int) = new TPRef(id, this.delay + delay)
    def -(delay: Int) = new TPRef(id, this.delay - delay)

    def <=(other: TPRef) = TBefore(this, other)
    def <(other: TPRef)  = TBefore(this, other + 1)
    def >=(other: TPRef) = other <= this
    def >(other: TPRef)  = other < this
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

    def findTimepoint(id: String): Option[TPRef] =
      elems
        .collect { case x: TPRef if x.id == id => x }
        .headOption
        .orElse(parent.flatMap(_.findTimepoint(id)))

    def findType(id: String): Option[Type] =
      elems.collect { case t@Type(`id`, _) => t }
      .headOption
      .orElse(parent.flatMap(_.findType(id)))
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
      "module:\n" + elems.map("  "+_).reverse.mkString("\n")
  }

}
