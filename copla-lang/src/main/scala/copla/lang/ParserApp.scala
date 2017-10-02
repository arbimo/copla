package copla.lang

import java.io.File

import copla.lang.model.format.Formatter
import copla.lang.model.{core, full}

sealed trait Mode
object ParsingMode extends Mode
object AbstractionHierarchyMode extends Mode

case class Config(mode: Mode = ParsingMode, anmlFile: File = new File("."), full: Boolean = false)

object ParserApp extends App {

  val optionsParser = new scopt.OptionParser[Config]("copla-lang") {
    head("""copla-lang is a set of libraries and command line utilities
        |to manipulate ANML files in the CoPla project.
        |""".stripMargin)

    opt[Unit]("full")
      .text("Do not translate the model to ANML core.")
      .action((_, c) => c.copy(full = true))

    arg[File]("anml-file")
      .text("ANML file to parse.")
      .action((f, c) => c.copy(anmlFile = f))

    cmd("fluent-hierarchy")
      .text("Displays an a hierarchy of the fluents in the domain, corresponding to Knoblock's abstraction hierarchies.")
      .action((_, c) => c.copy(mode = AbstractionHierarchyMode))
      .children(
        checkConfig(c => if(c.mode == AbstractionHierarchyMode && c.full) failure("fluent hierarchy cannot by checked on full model.") else success)
      )
  }

  def handleResult[T](res: Result[T], handler: T => Unit): Unit = {
    res match {
      case Success(model) =>
        handler(model)
      case ParseError(failure) =>
        println(failure.format)
      case x: Failure =>
        println(s"Crash: $x")
        sys.exit(1)
    }
  }

  optionsParser.parse(args, Config()) match {
    case Some(conf) if conf.mode == ParsingMode && conf.full =>
      handleResult(parseToFull(conf.anmlFile),
        (m: full.Model) => println(Formatter[full.Model].format(m)))
    case Some(conf) if conf.mode == ParsingMode =>
      handleResult(parse(conf.anmlFile),
        (m: core.CoreModel) => println(Formatter[core.CoreModel].format(m)))
    case Some(conf) if conf.mode == AbstractionHierarchyMode =>
      handleResult(parse(conf.anmlFile),
        (m: core.CoreModel) => {
          println(
            analysis.abstractionHierarchy(m)
              .toSeq
              .map { case (fluent, lvl) => s"$lvl $fluent"}
              .sorted
              .mkString("\n"))
        })
    case None =>
      sys.exit(1)
  }
}
