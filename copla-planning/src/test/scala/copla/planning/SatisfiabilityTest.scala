package copla.planning

import copla.constraints.meta.search.{NoSolution, SearchResult, Solution, TreeSearch}
import copla.constraints.meta.types.statics.TypedVariable
import copla.constraints.meta.variables.IntVariable
import copla.constraints.meta.{CSP, Configuration}
import copla.lang.model.core
import copla.planning.events.{InitPlanner, PlanningHandler}
import copla.planning.model.Problem
import org.scalatest.FunSuite
import slogging.{LogLevel, LoggerConfig}

class SatisfiabilityTest extends FunSuite {
  LoggerConfig.factory = slogging.SLF4JLoggerFactory()
  LoggerConfig.level = LogLevel.WARN



  test("Single sat/unsat (for debugging)") {
    testSat(16)
  }

  for (i <- Instances.satisfiables.indices) {
    test(s"satisfiability #${i + 1}") {
      testSat(i + 1)
    }
  }

  for (i <- Instances.unsatisfiables.indices) {
    test(s"unsatisfiability #${i + 1}") {
      testUnsat(i + 1)
    }
  }

  def testSat(i: Int) {
    val pb = Instances.satisfiables(i - 1)
    println(pb)
    val res = plan(pb)
    assert(res.isInstanceOf[Solution], res.toString)
    implicit val csp = res.asInstanceOf[Solution].csp
    assert(csp.isSolution, csp.report)

    def isInteresting(v: IntVariable) = v.ref match {
      case Some(_: core.Instance) => false
      case _                      => true
    }

    // print important variables
    val vars = csp.constraints.all
      .flatMap(c => c.variables(csp))
      .collect { case v: IntVariable if isInteresting(v) => v }
      .toSet
    for (v <- vars) v match {
      case v: TypedVariable[_] => println(s"$v = ${v.dom}")
      case _                   => println(s"$v = ${v.domain}")
    }
    println(s"end in ${csp.temporalHorizon.domain}")
    println(csp.getHandler(classOf[PlanningHandler]).report)
  }

  def testUnsat(i: Int) {
    val pb = Instances.unsatisfiables(i - 1)
    println(pb)
    plan(pb) match {
      case NoSolution => // ok
      case res@Solution(csp) =>
        println("\n -------- REPORT --------\n")
        println(csp.report)

        println(csp.getHandler(classOf[PlanningHandler]).report)
        fail(s"Expected NoSolution, got $res")
      case x =>
        fail(s"Expected NoSolution, got: $x")
    }
  }

  def plan(pbString: String): SearchResult = {
    Utils.plan(Utils.csp(Problem(pbString)))
  }

}
