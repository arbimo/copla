package copla.lang.model.transforms

import org.scalatest.FunSuite

class ActionInstantiationTest extends FunSuite {

  import copla.lang.model.core._
  import landscaper._

  import copla.lang

  test("action instantiation") {
    lang.parse("""
      |fluent boolean sv;
      |action Template(boolean x, boolean y) {
      |  duration := 10;
      |  [all] sv == true;
      |  [all] contains id: sv == x :-> y;
      |};
    """.stripMargin) match {
      case lang.Success(model) =>
        model.collectFirst { case x: ActionTemplate => x } match {
          case Some(act) =>
            val name = "myInstance"
            val instance = act.instance(name)
            assert(instance.name == name)

            // check that the name template has disappeared
            val stringWithAPattern = pattern { case x: String if x.contains("Template") => Seq(x) }
            assert(landscaper.extract(stringWithAPattern, instance.content).isEmpty)

            assert(instance.args.size == act.args.size)
            assert(instance.args.map(_.typ) == act.args.map(_.typ))
            assert(instance.template == act)

            println(instance.content.mkString("\n"))
          case None =>
            fail()
        }
      case _ =>
        fail()
    }
  }

}
