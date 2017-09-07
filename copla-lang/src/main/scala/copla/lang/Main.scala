package copla.lang

import java.io.File

import copla.lang.parsing.anml.{ParseFailure, ParseSuccess, Parser}

object Main extends App {

  case class Config(anmlFile: File = new File("."), quiet: Boolean = false)

  val optionsParser = new scopt.OptionParser[Config]("copla-lang") {
    head("""copla-lang is a set of libraries and command line utilities
        |to manipulate ANML files in the CoPla project.\n""".stripMargin)

    opt[Unit]('q', "quiet")
      .text("Do not print the result of parsing.")
      .action((_, c) => c.copy(quiet = true))

    arg[File]("anml-file")
      .text("ANML file to parse.")
      .action((f, c) => c.copy(anmlFile = f))
  }

  optionsParser.parse(args, Config()) match {
    case Some(conf) =>
      Parser.parseFromFile(conf.anmlFile) match {
        case ParseSuccess(module) =>
          if (!conf.quiet)
            println(module)
        case fail: ParseFailure =>
          println(fail.format)
          sys.exit(1)
      }
    case None =>
      sys.exit(1)
  }
}
