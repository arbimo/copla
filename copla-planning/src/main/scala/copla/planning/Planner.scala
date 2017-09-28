package copla.planning

import java.io.File

import copla.constraints.meta.search.{BinarySearch, SearchResult, Solution, TreeSearch}
import copla.constraints.meta.{CSP, Configuration}
import copla.planning.events.{InitPlanner, PlanningHandler}
import copla.planning.model.Problem


case class Config(file: File = new File("."))

object Planner extends App {

  slogging.LoggerConfig.factory = slogging.SLF4JLoggerFactory()
  slogging.LoggerConfig.level = slogging.LogLevel.INFO

  val parser = new scopt.OptionParser[Config]("copla") {
    head("copla")

    arg[File]("anml-problem-file")
      .action((x, c) => c.copy(file = x))
      .text("ANML problem file for this planner to solve.")

    help("help").text("prints this usage text")
  }

  val conf: Config = parser.parse(args, Config()) match {
    case Some(x) => x
    case None    => System.exit(1); null
  }

  val csp = Problem.from(conf.file)
    .map(Utils.csp) match {
    case copla.lang.Success(csp) =>
      csp
    case err =>
      println(err)
      sys.exit(1)
  }

  val planningResult = Utils.plan(csp)

  planningResult match {
    case Solution(solution) =>
      println(solution.getHandler(classOf[PlanningHandler]).report)
    case x =>
      println(x)
      println("No solution found.")
  }
}

object Utils {

  def csp(pb: Problem): CSP = {
    val csp = new CSP(Left(new Configuration(enforceTpAfterStart = false)))
    csp.addHandler(new PlanningHandler(csp, Left(pb)))
    csp.addEvent(InitPlanner)
    csp
  }

  def plan(csp: CSP): SearchResult = {
    val searcher = new TreeSearch(List(csp))
    searcher.incrementalDeepeningSearch()
//    BinarySearch.search(csp)
  }
}
