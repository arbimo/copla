package copla.constraints.meta.typing

import copla.constraints.meta.CSP
import copla.constraints.meta.types.statics.{BaseType, ComposedType, TypedVariable}
import org.scalatest.{BeforeAndAfter, FunSuite}

class TypeTest extends FunSuite with BeforeAndAfter {
  implicit var csp: CSP = null

  before {
    csp = new CSP
  }

  test("simple type") {
    val t = BaseType("base", List("a", "b", "c"))
    val v1 = new TypedVariable("v1", t)
    val v2 = new TypedVariable("v2", t)

    csp.post(v1 === "a")
    csp.post(v2 =!= v1)
    csp.propagate()

    assert(v1.dom.contains("a"))
    assert(!v2.dom.contains("a"))

    println(v2.dom.values)
  }

  test("type hierarchy") {
    class AB
    class A extends AB
    class B extends AB
    val a1 = new A
    val a2 = new A
    val b1 = new B

    val AT = new BaseType[A]("A", List((a1, 0), (a2, 1)))
    val BT = new BaseType[B]("B", List((b1, 2)))
    val ABT = new ComposedType[AB](List(AT, BT))

    println(ABT.instances)
    assert(ABT.hasInstance(a1))
    assert(ABT.hasInstance(a2))
    assert(ABT.hasInstance(b1))
  }

  test("type hierarchy with instances in top type") {
    class AB
    class A extends AB
    class B extends AB
    val a1 = new A
    val a2 = new A
    val b1 = new B
    val ab1 = new AB
    val ab2 = new AB

    val AT = new BaseType[A]("A", List((a1, 0), (a2, 1)))
    val BT = new BaseType[B]("B", List((b1, 2)))
    val ABT = new ComposedType[AB](List(AT, BT), List((ab1, 3), (ab2, 4)))

    println(ABT.instances)
    assert(ABT.hasInstance(a1))
    assert(ABT.hasInstance(a2))
    assert(ABT.hasInstance(b1))
    assert(ABT.hasInstance(ab1))
    assert(ABT.hasInstance(ab2))
    assert(ABT.instanceToInt(ab2) == 4)
    assert(ABT.instanceToInt(a1) == 0)
    assert(ABT.intToInstance(0) == a1)
    assert(ABT.intToInstance(3) == ab1)
  }
}
