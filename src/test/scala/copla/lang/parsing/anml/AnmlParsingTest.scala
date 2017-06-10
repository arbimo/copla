package copla.lang.parsing.anml

import fastparse.core.Parsed.Success
import org.scalatest.FunSuite

class AnmlParsingTest extends FunSuite {

  def valid = Seq(
    "type a;",
    " type a ; ",
    "type b2;",
    "type a; type b < a;"
  )

  def invalid = Seq(
    "type 2b;",
    "type type;",
    "type a < a;",
    "type a; type a;",
    "type a < b;"
  )

  for(anml <- valid) {
    test("valid: "+anml) {
      Parser.parse(anml) match {
        case Success(module, _) =>
          println("PARSED:")
          println(anml)
          println("\nAS:")
          println(module)
        case x =>
          fail(s"Could not parse anml string: $x\n\n$anml\n")
      }
    }
  }

  for(anml <- invalid) {
    test("invalid: "+anml) {
      Parser.parse(anml) match {
        case Success(module, _) =>
          fail(s"Following anml string should be invalid:\n$anml\n\nParsed to:\n $module")
        case x =>
          println(s"Detected invalid ANML: $x\n$anml")
      }
    }
  }


  val tmp = "type a < a;"
  test("debug: tmpoary") {
    println(tmp)
    println(Parser.parse(tmp))
  }
}
