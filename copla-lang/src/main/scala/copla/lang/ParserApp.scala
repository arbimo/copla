package copla.lang

import java.io.File

import copla.lang.parsing.anml.{GenFailure, ParseFailure, ParseSuccess, Parser}

case class Config(anmlFile: File = new File("."),
                  quiet: Boolean = false)

object ParserApp extends App {

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
      Parser.parse(conf.anmlFile) match {
        case ParseSuccess(model) =>
          if (!conf.quiet)
            println(model)
        case x: GenFailure =>
          println(x.format)
          sys.exit(1)
      }
    case None =>
      sys.exit(1)
  }
}
