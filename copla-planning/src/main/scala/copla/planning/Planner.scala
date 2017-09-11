package copla.planning

import java.io.File

import copla.constraints.meta.search.TreeSearch
import copla.constraints.meta.{CSP, Configuration}
import copla.lang.model.Model
import copla.lang.parsing.anml.Parser
import copla.planning.events.{InitPlanner, PlanningHandler}
import copla.planning.model.Problem

case class Config(file: File = new File("."))

object Planner extends App {

  val parser = new scopt.OptionParser[Config]("lcp") {
    head("lcp")

    arg[File]("anml-problem-file")
      .action((x, c) => c.copy(file = x))
      .text("ANML problem file for this planner to solve.")

    help("help").text("prints this usage text")
  }

  val conf: Config = parser.parse(args, Config()) match {
    case Some(x) => x
    case None => System.exit(1); null
  }

  val pb = Utils.problem(conf.file)
  val csp = Utils.csp(pb)
  val searcher = new TreeSearch(List(csp))
  searcher.incrementalDeepeningSearch() match {
    case Left(solution) =>
      println(solution.getHandler(classOf[PlanningHandler]).report)
    case _ =>
      println("No solution found.")
  }

}

object Utils {

  def problem(anmlProblemFile: File): Problem = {
    new Problem(Model()) // todo: actually parse the file
  }

  def csp(pb: Problem) : CSP = {
    val csp = new CSP(Left(new Configuration(enforceTpAfterStart = false)))
    csp.addHandler(new PlanningHandler(csp, Left(pb)))
    csp.addEvent(InitPlanner)
    csp
  }
}
