package copla.exp

import java.io.{File, PrintWriter}

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.search._
import copla.lang.analysis.{DetailedKnoblock, Knoblock}
import copla.planning._
import copla.planning.events.PlanningHandler
import copla.planning.model.Problem
import copla.planning.search.DecisionFeatures.{Extractor, Feature}
import copla.planning.search.DecisionOrderings.SeqFeatures
import copla.planning.search.{DecisionFeatures, DecisionOrderings}

import scala.collection.mutable
import scala.collection.immutable

object SearchSpace extends App {


  slogging.LoggerConfig.factory = slogging.PrintLoggerFactory()
  slogging.LoggerConfig.level = slogging.LogLevel.WARN

  val numTentatives = 100
  val numOrderings = 8

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

      val successCount = (0 until numTentatives).par.map(i => {
        val searcher: Searcher = csp => GreedySearcher.search(csp, decisionOrdering, OptionPicker.randomized(i), ctx => ctx.depth > 100)
        searcher.search(csp) match {
          case x@Solution(sol) => true
          case NoSolutionFound(_) => false
          case x => Console.err.println(x.toString); false
        }
      }).count(x => x)

      println(s"${decisionOrdering(csp)}: $successCount / $numTentatives <- ${pbFile.getName}")
      results += ((decisionOrdering(csp).toString, pbFile.getName, successCount))
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


  def randomDecisionOrdering(i: Int, csp: CSPView): CSP => DecisionOrdering = {

    import DecisionFeatures.extractors._

    val features: CSP => Seq[Extractor] = csp =>  i match {
      case 0 => noChoice :: numOptions :: isSupport :: absLvl(csp, Knoblock) :: Nil
      case 1 => noChoice :: numOptions :: isSupport :: absLvl(csp, DetailedKnoblock) :: Nil
      case 2 => noChoice :: numOptions :: isNotSupport :: Nil
      case 3 => noChoice :: isSupport :: absLvl(csp, Knoblock) :: numOptions :: Nil
      case 4 => noChoice :: isSupport :: absLvl(csp, DetailedKnoblock) :: numOptions :: Nil
      case 5 => noChoice :: isNotSupport ::  numOptions :: Nil
      case 6 => isSupport :: noChoice :: absLvl(csp, DetailedKnoblock) :: numOptions :: Nil
      case 7 => noChoice :: isNotSupport :: numOptions :: absLvl(csp, DetailedKnoblock) :: Nil
    }

    csp => SeqFeatures(features(csp))
  }
}


object DataUtils extends App {
  import shapeless.HList

  case class Row(name: Any, data: Seq[Any]) {
    def apply(i: Int): Any = data(i)
  }

  case class Table(columns: Seq[Any], content: immutable.Seq[Row] = Vector()) {

    def focus(columnsName: Any, contentName: Any): Table = {
      val columnsIndex = columns.indexOf(columnsName)
      val target = columns.indexOf(contentName)

      val map: mutable.Map[(Any, Any), mutable.ArrayBuffer[Any]] = mutable.Map()
      for(r <- content) {
        map.getOrElseUpdate(r.name -> r(columnsIndex), mutable.ArrayBuffer()) += r(target)
      }
      val rowNames = content.map(_.name).distinct

      val table = Table(column(columnsName).map(_.toString).distinct)

      val newRows = for(row <- rowNames) yield {
        Row(row, column(columnsName).map(_.toString).distinct.map(c => map.getOrElse(row -> c, Nil)))
      }
      newRows.foldLeft(table)((t, row) => t + row)
    }

    def column(name: Any): immutable.Seq[Any] = {
      val i = columns.indexOf(name)
      content.map(row => row.data(i))
    }

    def +(row: Row): Table = Table(columns, content :+ row)
    def withRow(values: Any*): Table = this + Row(content.size.toString, values)

    override def toString: String = {
      val maxSizes = columns.map(c => column(c).map(_.toString.length).max)
      val sb = new mutable.StringBuilder()
      for(r <- content) {
        for(i <- r.data.indices) {
          sb ++= r(i).toString.padTo(maxSizes(i), ' ')
          sb ++= " "
        }
        sb ++= "\n"
      }
      sb.toString()
    }
  }

  val t = Table("a" :: "b" :: "c" :: Nil) +
    Row(1, Seq(1, 3)) +
    Row(1, Seq(2, 4))

  val t2 = t.focus("b", "c")
  println(t)
  println(t2)
  println("BREAK")
}
