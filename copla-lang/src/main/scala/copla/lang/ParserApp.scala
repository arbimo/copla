package copla.lang

import java.io.File

import copla.lang.model.format.Formatter

case class Config(anmlFile: File = new File("."), quiet: Boolean = false, full: Boolean = false)

object ParserApp extends App {

  val optionsParser = new scopt.OptionParser[Config]("copla-lang") {
    head("""copla-lang is a set of libraries and command line utilities
        |to manipulate ANML files in the CoPla project.
        |""".stripMargin)

    opt[Unit]('q', "quiet")
      .text("Do not print the result of parsing.")
      .action((_, c) => c.copy(quiet = true))

    opt[Unit]("full")
      .text("Do not translate the model to ANML core.")
      .action((_, c) => c.copy(full = true))

    arg[File]("anml-file")
      .text("ANML file to parse.")
      .action((f, c) => c.copy(anmlFile = f))
  }

  def handleResult[T](res: Result[T], conf: Config)(implicit fmt: Formatter[T]): Unit = {
    res match {
      case Success(model) if !conf.quiet =>
        println(fmt.format(model))
      case Success(_) =>
      case ParseError(failure) =>
        println(failure.format)
      case x: Failure =>
        println(s"Crash: $x")
        sys.exit(1)
    }
  }

  optionsParser.parse(args, Config()) match {
    case Some(conf) if conf.full =>
      handleResult(parseToFull(conf.anmlFile), conf)
    case Some(conf) =>
      handleResult(parse(conf.anmlFile), conf)
    case None =>
      sys.exit(1)
  }
}
