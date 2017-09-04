package copla.lang

import copla.lang.parsing.anml.Parser
import fastparse.core.Parsed.Success

object Main extends App {
  if(args.length != 1) {
    println("Error: should provide the name of an ANML file.")
    System.exit(1)
  } else {
    val source = scala.io.Source.fromFile(args(0))
    val input = source.getLines.mkString("\n")
    Parser.parse(input) match {
      case Success(module, _) =>
        println("AS:\n" + module + "\n\n")
      case x =>
        println(s"Could not parse anml string: \n$x")
        System.exit(1)
    }
  }
}
