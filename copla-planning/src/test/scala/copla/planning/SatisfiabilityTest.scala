package copla.planning

import copla.constraints.meta.search.{Solution, TreeSearch}
import copla.constraints.meta.types.statics.TypedVariable
import copla.constraints.meta.variables.IntVariable
import copla.constraints.meta.{CSP, Configuration}
import copla.lang.model.core
import copla.planning.events.{InitPlanner, PlanningHandler}
import copla.planning.model.Problem
import org.scalatest.FunSuite
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}

class SatisfiabilityTest extends FunSuite {
  LoggerConfig.factory = slogging.SLF4JLoggerFactory()
  System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "TRACE")
  LoggerConfig.level = LogLevel.DEBUG



  test("Single sat/unsat (for debugging)") {
    testSat(7)
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
    implicit val csp = plan(pb)
    assert(csp != null)
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
    implicit val csp = plan(pb)
    if (csp != null) {
      println("\n -------- REPORT --------\n")
      println(csp.report)

      println(csp.getHandler(classOf[PlanningHandler]).report)
    }
    assert(csp == null)
  }

  def plan(pbString: String): CSP = {
    Utils.plan(Utils.csp(Problem(pbString))) match {
      case Left(solution) => solution
      case x =>
        println(x)
        null
    }
  }

}
