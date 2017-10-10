package copla.constraints.meta

import copla.constraints.meta.constraints.{Contradiction, Tautology}
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.variables.IntVariable
import org.scalatest.{BeforeAndAfter, FunSuite}
import copla.constraints.meta.search.{BinarySearch, Solution}

class DisjunctiveConstraintTest extends FunSuite with BeforeAndAfter {


  slogging.LoggerConfig.factory = slogging.PrintLoggerFactory()
  slogging.LoggerConfig.level = slogging.LogLevel.WARN

  implicit var csp: CSP = null
  var v1, v2, v3: IntVariable = null

  before {
    csp = new CSP
    v1 = csp.variable("v1", Set(1,2,3))
    v2 = csp.variable("v2", Set(2, 3))
    v3 = csp.variable("v3", Set(1, 2, 3))
  }

  test("Disjunction constraint enforced by search") {
    csp.post(v2 =!= v1 || v2 =!= v3)
    csp.propagate()
    val res = BinarySearch.search(csp)
    res match {
      case Solution(sol) =>
        println(sol.report)
        assert(sol.isSolution)
      case _ =>
        assert(false)
    }
  }

  test("Same constraint posted and watched") {
    val c = v1 =!= v2
    val c2 = v1 === v2
    val mc = (c || c2) && (v1 =!= v3)
    csp.post(mc)
    val res = BinarySearch.search(csp)
    res match {
      case Solution(sol) =>
        println(sol.report)
        assert(sol.isSolution)
      case _ =>
        assert(false)
    }
  }
}
