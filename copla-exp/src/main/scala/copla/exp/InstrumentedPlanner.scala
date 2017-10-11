package copla.exp

import java.io.File

import copla.constraints.meta.search._
import copla.planning.Utils
import copla.planning.events.PlanningHandler
import copla.planning.model.Problem
import copla.planning.search.DecisionOrderings


object InstrumentedPlanner extends App {

  slogging.LoggerConfig.factory = slogging.SLF4JLoggerFactory()
  slogging.LoggerConfig.level = slogging.LogLevel.DEBUG

  //  val pbFile = new File(defaultExpDir, "handover-flat.p01.pb.anml")
  //  val pbFile = new File(defaultExpDir, "blocks_ipc2.p04-0.pb.anml")
    val pbFile = new File(defaultExpDir, "docks-flat.p1.pb.anml")

  val baseCsp = Problem.from(pbFile)
      .map(Utils.csp) match {
      case copla.lang.Success(csp) =>
        csp
      case copla.lang.ParseError(failure) =>
        println(failure.format)
        sys.exit(1)
      case err =>
        println(err)
        sys.exit(1)
    }

  val csp = baseCsp.clone
  csp.addHandler(new Instrumentation())

  val searcher: Searcher = csp =>
    GreedySearcher.search(csp, DecisionOrderings.default, OptionPicker.randomized(0), ctx => ctx.depth > 10)

  val lastCSP = searcher.search(csp) match {
    case x@Solution(sol) =>
      sol
    case NoSolutionFound(Some(csp)) =>
      println(csp.report)
      println(csp.getHandler(classOf[PlanningHandler]).report)
      csp
    case x => sys.error(x.toString)
  }

  println(
    lastCSP.getHandler(classOf[Instrumentation]).eventsClassCount.toSeq
      .sortBy(_._2)
      .reverse
      .map { case (event, count) => "%7d".format(count)+"  "+event }
      .mkString("\n")
  )

  println(
    lastCSP.getHandler(classOf[Instrumentation]).eventsCount.toSeq
      .filter(_._2 > 10)
      .sortBy(_._2)
      .map { case (event, count) => "%7d".format(count)+"  "+event }
      .mkString("\n")
  )

//  val numTests = 100
//  val startTime = System.currentTimeMillis()
//  for(i <- 0 to numTests)
//    searcher.search(baseCsp)
//  val endTime = System.currentTimeMillis()
//  val runtime = (endTime - startTime) / numTests

//  println(s"Runtime: $runtime")

}
