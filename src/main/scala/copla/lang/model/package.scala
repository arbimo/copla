package copla.lang

import exception._

package object model {

  trait Elem
  trait ModuleElem extends Elem
  trait Statement extends ModuleElem
  trait TimedSymExpr extends Elem

  case class Type(name: String, parent: Option[Type]) extends ModuleElem {

    def apply(argumentName: String): Arg = Arg(this, argumentName)

    def <(parent: Type): Type = {
      assert1(
        this.parent.isEmpty,
        s"Type '$name' already has a super type (${this.parent.get}) when trying to make it inherit $parent")
      Type(name, Some(parent))
    }

    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))
  }

  case class Arg(typ: Type, name: String)

  case class StateVariableTemplate(name: String, typ: Type, params: Seq[Arg]) extends ModuleElem {

    def apply(variables: TVariable*): StateVariable = {
      require(params.size == variables.size,
              s"Wrong number of arguments for state variable, $name")
      StateVariable(this, variables)
    }
  }

  case class StateVariable(template: StateVariableTemplate, params: Seq[TVariable]) extends ModuleElem {
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

  case class TVariable(id: String, typ: Type) extends ModuleElem

  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: Int) = to <= from + dur
    def <(dur: Int)  = to < from + dur
    def >=(dur: Int) = to >= from + dur
    def >(dur: Int)  = to > from + dur
    def +(time: Int) = Delay(from, to + 1)
    def -(time: Int) = Delay(from, to - 1)
  }

  class TPRef(val id: String, val delay: Int = 0) {

    override def toString = id.toString()

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

  trait Ctx {
    def elems: Seq[Elem]
    def parent: Option[Ctx]

    def findVariable(id: String): Option[TVariable] =
      elems
        .collect { case x @ TVariable(`id`, _) => x }
        .headOption
        .orElse(parent.flatMap(_.findVariable(id)))

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

  case class Mod(elems: Seq[ModuleElem] = Seq()) extends Ctx {
    override def parent = None
    def +(elem: ModuleElem): Option[Mod] = {
      Some(Mod(elem +: elems))
    }

    def ++(elems: Seq[ModuleElem]): Option[Mod] = {
      elems.foldLeft(Option(this))((m, elem) => m.flatMap(_ + elem))
    }

    override def toString =
      "module:\n" + elems.map("  "+_).reverse.mkString("\n")
  }

}
