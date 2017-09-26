package copla.lang.model.transforms

import copla.lang.model.core._
import copla.lang.parsing.anml.{InputAnmlModels, ParseFailure, ParseSuccess, Parser}
import org.scalatest.FunSuite

class FullToCoreTest extends FunSuite {

  def asCore(anml: String): Either[String,CoreModel] = {
    Parser.parse(anml) match {
      case ParseSuccess(mod) =>
        Right(FullToCore.trans(mod))
      case _ =>
        Left("Could not parse anml string")
    }
  }

  for (anml <- InputAnmlModels.valid) {
    test("translation to core: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(module) =>
          println(FullToCore.trans(module).mkString("\n"))
        case x: ParseFailure =>
        // ignore, this should be caught by the ANML parsing tests
      }
    }
  }

  for (anml <- InputAnmlModels.valid) {
    test("Invariant on core: " + anml) {
      Parser.parse(anml) match {
        case ParseSuccess(fullMod) =>
          val m = FullToCore.trans(fullMod)
          checkInvariantsInCore(m)
        case _ =>
          fail("Could not parse anml string")
      }
    }
  }

  def checkInvariantsInCore(m: CoreModel): Unit = {
    val declarations = m.collect { case x: Declaration[_] => x }
    assert(declarations.distinct.size == declarations.size,
      "\nDuplicate declarations in: \n"+declarations.mkString("\n"))
  }
}
