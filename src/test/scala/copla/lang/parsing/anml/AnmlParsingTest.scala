package copla.lang.parsing.anml

import fastparse.core.Parsed.Success
import org.scalatest.FunSuite

class AnmlParsingTest extends FunSuite {

  def valid = Seq(
    "type a;",
    " type a ; ",
    "type b2;",
    "type a; type b < a;",
    "type A; instance A a;",
    "type A; instance A a, b, c;",
    "type A;  instance   A  a ,b , c,d, e ; ",
    "type A; instance A a1, a2; type B < A; instance A a3; instance B b1, b2;"
  )

  def invalid = Seq(
    "type 2b;",
    "type type;",
    "type a < a;",
    "type a; type a;",
    "type a < b;",
    "type A; instance B a;",
    "type A; instance A;",
    "type A; instance A a, a;",
    "type A; instance A A;"
  )

  for(anml <- valid) {
    test("valid: "+anml) {
      Parser.parse(anml) match {
        case Success(module, _) =>
          println("PARSED:\n"+anml+"\n")
          println("AS:\n"+module+"\n\n")
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
  test("debug: temporary") {
    /** Dummy text to facilitate testing. */
    println(tmp)
    println(Parser.parse(tmp))
  }
}
