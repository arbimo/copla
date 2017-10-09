package copla.exp

import java.io.{File, PrintWriter}

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.search._
import copla.planning._
import copla.planning.events.PlanningHandler
import copla.planning.model.Problem
import copla.planning.search.DecisionFeatures.{Extractor, Feature}
import copla.planning.search.DecisionOrderings.SeqFeatures
import copla.planning.search.{DecisionFeatures, DecisionOrderings}

import scala.collection.mutable

object SearchSpace extends App {


  slogging.LoggerConfig.factory = slogging.PrintLoggerFactory()
  slogging.LoggerConfig.level = slogging.LogLevel.WARN

  val numTentatives = 20
  val numOrderings = 6

  val results: mutable.ArrayBuffer[(String, String, Int)] = mutable.ArrayBuffer()

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

    for (ord <- 0 until numOrderings) {
      val decisionOrdering = randomDecisionOrdering(ord, csp)

      var successCount = 0
      for (i <- 0 until numTentatives) {
        val searcher: Searcher = csp => GreedySearcher.search(csp, decisionOrdering, OptionPicker.randomized(i), ctx => ctx.depth > 100)

        searcher.search(csp) match {
          case x@Solution(sol) =>
            successCount += 1
          case NoSolutionFound(_) => // keep searching
          case x => sys.error(x.toString)
        }
      }
      println(s"$decisionOrdering: $successCount / $numTentatives <- ${pbFile.getName}")
      results += ((decisionOrdering.toString, pbFile.getName, successCount))
    }
  }

  val numBenchmark: Int = numTentatives * problems().size

  println("\nAggregated: ")
  val formatted =
    results
      .groupBy(_._1)
      .mapValues(_.map(_._3).sum).toSeq
      .sortBy(_._2)
      .reverse
      .map(p => f"${p._2}%5d / $numBenchmark  ${p._1}")
      .mkString("\n")

  println(formatted)
  new PrintWriter("/tmp/search-space.txt") { try { write(formatted) } finally {close} }


  def randomDecisionOrdering(i: Int, csp: CSPView): DecisionOrdering = {
    val abstractionLevel = copla.lang.analysis.abstractionHierarchy(
      csp.getHandler(classOf[PlanningHandler]).pb.anml
    )

    val absLvl = DecisionFeatures.abstractLevels(f => abstractionLevel(f)).withName("abs-lvl")
    import DecisionFeatures.extractors._
    val features: Seq[Extractor] = i match {
      case 0 => noChoice :: numOptions :: isSupport :: absLvl :: Nil
      case 1 => noChoice :: numOptions :: isNotSupport :: absLvl :: Nil
      case 2 => noChoice :: isSupport :: absLvl :: numOptions :: Nil
      case 3 => noChoice :: isNotSupport ::  numOptions :: absLvl :: Nil
      case 4 => isSupport :: noChoice :: absLvl :: numOptions :: Nil
      case 5 => noChoice :: isNotSupport :: numOptions :: absLvl :: Nil
    }

    SeqFeatures(features)
  }
}
