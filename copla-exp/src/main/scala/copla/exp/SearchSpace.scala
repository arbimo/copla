package copla.exp

import java.io.File

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.search._
import copla.planning._
import copla.planning.events.PlanningHandler
import copla.planning.model.Problem

object SearchSpace extends App {


  val numTentatives = 30

  for(pbFile <- problems()) {
    val csp = Problem.from(pbFile)
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

    var successCount = 0
    for(i <- 0 until numTentatives) {
      val searcher: Searcher = csp => GreedySearcher.search(csp, OptionPicker.randomized(i), ctx => ctx.depth > 100)

      searcher.search(csp) match {
        case x@Solution(sol) =>
          successCount += 1
        case NoSolutionFound(_) => // keep searching
        case x => sys.error(x.toString)
      }
    }
    println(s"$successCount / $numTentatives <- ${pbFile.getName}")
  }

}
