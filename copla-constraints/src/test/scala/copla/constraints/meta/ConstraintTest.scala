package copla.constraints.meta

import copla.constraints.meta.constraints.{EqualityConstraint, InequalityConstraint, Satisfied, UpdateDomain}
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events.NewConstraint
import copla.constraints.meta.variables.IntVariable
import org.scalatest.{BeforeAndAfter, FunSuite}

class ConstraintTest extends FunSuite with BeforeAndAfter {

  implicit var csp: CSP = null
  var v1, v2: IntVariable = null

  before {
    csp = new CSP
    v1 = csp.variable("v1", Set(1,2,3))
    v2 = csp.variable("v2", Set(2))
  }

  test("Constraint creation") {
    val c1 = v1 === v2
    assert(c1.isInstanceOf[EqualityConstraint])
    assert(c1.v1 == v1)
    assert(c1.v2 == v2)

    val c2 = v1 =!= v2
    assert(c2.isInstanceOf[InequalityConstraint])
    assert(c2.v1 == v1)
    assert(c2.v2 == v2)
  }

  test("Constraint propagation difference") {
    val c = v1 =!= v2
    val res = c.propagate(NewConstraint(c))
    assert(res == Satisfied(UpdateDomain(v1, Domain(1,3))))
  }

  test("Constraint propagation difference in CSP") {
    csp.post(v1 =!= v2)
    csp.propagate()

    assert(v1.domain.size == 2)
    assert(!v1.domain.contains(2))
  }

  test("Constraint propagation equality") {
    val c = v1 === v2
    val res = c.propagate(NewConstraint(c))

    assert(res == Satisfied(UpdateDomain(v1, Domain(2))))
  }

  test("Constraint propagation equality in CSP") {
    csp.post(v1 === v2)
    csp.propagate()

    assert(v1.domain.size == 1)
    assert(v1.domain.contains(2))
  }

}
