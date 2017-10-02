package copla.planning

import java.io.File

import copla.constraints.meta.search._
import copla.constraints.meta.{CSP, CSPView, Configuration}
import copla.planning.events.{InitPlanner, PlanningHandler}
import copla.planning.model.Problem


case class Config(file: File = new File("."), tentatives: Int = 1000, seed: Int = 0, maxDepth: Int = 200)

object Planner extends App with slogging.StrictLogging {

  slogging.LoggerConfig.factory = slogging.SLF4JLoggerFactory()
  slogging.LoggerConfig.level = slogging.LogLevel.INFO

  val parser = new scopt.OptionParser[Config]("copla") {
    head("copla")

    opt[Int]('t', "tentatives")
      .action((x, c) => c.copy(tentatives = x))
      .text("Number of randomized searches to try")

    opt[Int]('s', "seed")
      .action((x, c) => c.copy(seed = x))
      .text("Seed of the first tentative. Seed for subsequent tentatives are obtained by incrementing this one.")

    opt[Int]('d', "max-depth")
      .action((x, c) => c.copy(maxDepth = x))
      .text("After at which the planner will abandon search.")

    arg[File]("anml-problem-file")
      .action((x, c) => c.copy(file = x))
      .text("ANML problem file for this planner to solve.")

    private val logOption = "{debug, info, warn, error}"
    opt[String]("log")
      .action((x, c) => {
        val lvl = x match {
          case "debug" => slogging.LogLevel.DEBUG
          case "info" => slogging.LogLevel.INFO
          case "warn" => slogging.LogLevel.WARN
          case "error" => slogging.LogLevel.ERROR
          case _ =>
            println(s"Error: log level should be in one of $logOption")
            sys.exit(1)
        }
        slogging.LoggerConfig.level = lvl
        c
      })

    help("help").text("prints this usage text")
  }

  val conf: Config = parser.parse(args, Config()) match {
    case Some(x) => x
    case None    => sys.exit(1)
  }

  val csp = Problem.from(conf.file)
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

  val searcher: Searcher = new Searcher {
    override def search(csp: CSP): SearchResult = {
      var bestSolution: Option[CSP] = None
      def numAction: CSPView => Int = csp =>         csp.getHandler(classOf[PlanningHandler]).actions.size
      def stopCondition: GreedySearcher.Context => Boolean = ctx =>
        ctx.depth > conf.maxDepth || numAction(ctx.csp) >= bestSolution.map(numAction).getOrElse(Int.MaxValue)

      for (i <- 0 until conf.tentatives) {
        val subSearcher: Searcher = csp => GreedySearcher.search(csp, OptionPicker.randomized(conf.seed + i), stopCondition)
        subSearcher.search(csp) match {
          case x@Solution(sol) =>
            println(sol.getHandler(classOf[PlanningHandler]).report)
            assert(numAction(sol) < bestSolution.map(numAction).getOrElse(Int.MaxValue))
            bestSolution = Some(sol)
          case NoSolutionFound(_) => // keep searching
          case x => return x
        }
      }
      bestSolution match {
        case Some(s) => Solution(s)
        case None => NoSolutionFound(None)
      }
    }
  }

  val planningResult = searcher.search(csp)

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
