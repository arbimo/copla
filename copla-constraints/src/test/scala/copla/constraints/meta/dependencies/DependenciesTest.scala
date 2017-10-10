package copla.constraints.meta.dependencies

import copla.constraints.meta.CSP
import copla.constraints.meta.constraints.{Constraint, Tautology}
import org.scalatest.FunSuite

class DependenciesTest extends FunSuite {

  implicit val csp = new CSP()
  val v1 = csp.variable("v1", Set(1,2,3))
  val v2 = csp.variable("v2", Set(1,2,3))
  val v3 = csp.variable("v3", Set(1,2,3))


  val dep: Node => Set[Node] = _ match {
    case Var(v) => Set()
    case Cst(c) =>
      c.subconstraints.map(Cst(_)).toSet[Node] ++ c.variables.map(Var(_))
  }

  test("constraint addition") {
    val tree = new DependencyGraph()
    val c1 = v1 === v2 || v1 === v3
    val c2 = v3 === v2
    val c3 = c1 && c2

    implicit def constraint2Node(c: Constraint): Cst = Cst(c)


    tree.addEdge(Root, c3, dep)

    assert(tree.children(c3).contains(c1))
    assert(tree.children(c3).contains(c2))
    assert(tree.parents(c1).contains(c3))

    tree.addEdge(Root, c2, dep)

    assert(tree.parents(c2).contains(Root))
    assert(tree.children(Root).contains(c2))

    tree.removeEdge(Root, c3)

    assert(tree.parents(c2).contains(Root))
    assert(tree.children(Root).contains(c2))

    assert(!tree.contains(c3))
    assert(!tree.contains(c1))

    println(tree)

  }

}
