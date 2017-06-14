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
    "type A; instance A a1, a2; type B < A; instance A a3; instance B b1, b2;",
    "type A; type B; fluent A sv(A first, B second);",
    "type A; type B; fluent A f(B b); instance B b1;",
    "type A; fluent A sv;",
    "type A; fluent A sv();",
    "type A; instance A x; fluent A sv(A x);", // shadowing allowed (might benefit from warning)
    "start < end;",
    "duration == 10;",
    "end - start == 10;",
    "timepoint t; t < 100;",
    "start -10  ==  end+5;",
    "start -10 -end +2 < 0;",
    "type A; type B; fluent A f(B b); instance A a1; instance B b1; [start,end] f(b1) == a1;",
    "type A; type B; fluent A f(B b); instance A a1; instance B b1; [start,end] id: f(b1) == a1; [start+10, 20] f(b1) == a1;",
    "fluent boolean f; [10,10] id: f == true; id.start < 10;"
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
    "type A; instance A A;",
    "fluent A sv;",
    "type A; fluent A sv(B b);",
    "type A; fluent A sv(A)",
    "type A; instance A sv; fluent A sv(A x)",
    "start < x;",
    "timepoint t; start - end < t;",
    "type A; type B; fluent A f(A a); instance A a1; instance B b1; [start,end] f(a1) == b1;",
    "type A; type B; fluent A f(B b); instance A a1; instance B b1; [start,end] id: f(b1) == a2;"
  )

  for (anml <- valid) {
    test("valid: " + anml) {
      Parser.parse(anml) match {
        case Success(module, _) =>
          println("PARSED:\n" + anml + "\n")
          println("AS:\n" + module + "\n\n")
        case x =>
          fail(s"Could not parse anml string: $x\n\n$anml\n")
      }
    }
  }

  for (anml <- invalid) {
    test("invalid: " + anml) {
      Parser.parse(anml) match {
        case Success(module, _) =>
          fail(s"Following anml string should be invalid:\n$anml\n\nParsed to:\n $module")
        case x =>
          println(s"Detected invalid ANML: $x\n$anml")
      }
    }
  }

  val tmp =
    "type A; type B; fluent A f(B b); instance A a1; instance B b1; [start,end] f(b1) == a1;"
  test("debug: temporary") {

    /** Dummy text to facilitate testing. */
    println(tmp)
    println(Parser.parse(tmp))
  }
}
