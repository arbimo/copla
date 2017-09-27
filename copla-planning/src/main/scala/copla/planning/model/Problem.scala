package copla.planning.model

import java.io.File

import copla.lang.model.core._
import copla.lang.model.transforms.CoreTransforms.ConstantAsLocalVar
import copla.lang.model.transforms.{CoreTransforms, FullToCore}
import copla.lang.parsing.anml.{GenFailure, ParseSuccess, Parser}
import copla.planning.model.Problem.AnmlModel

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class Problem(val anml: AnmlModel) {

  val start: TPRef = anml
    .collectFirst { case TimepointDeclaration(tp) if tp.id.id == Id(RootScope, "start") => tp }
    .getOrElse(sys.error("No start timepoint in ANML module"))
  val end: TPRef = anml
    .collectFirst { case TimepointDeclaration(tp) if tp.id.id == Id(RootScope, "end") => tp }
    .getOrElse(sys.error("No end timepoint in ANML module"))

  lazy val actionTemplates: Seq[ActionTemplate] = anml.collect {
    case act: ActionTemplate => act
  }

  lazy val chronicle = new Chronicle(
    anml.collect { case s: Statement => s }
  )

  private val coverageTest = anml.map {
    case x: Statement      => null
    case x: Declaration[_] => null
  }
}
object Problem {
  type AnmlModel = Seq[InModuleBlock] with ConstantAsLocalVar

  private def apply(fullModel: copla.lang.model.full.Model): Problem = {
    val coreRaw  = FullToCore.trans(fullModel)
    val withVars = CoreTransforms.replaceConstantsWithLocalVars(coreRaw)

    new Problem(withVars)
  }

  def apply(anmlProblemFile: File): Problem = {
    Parser.parse(anmlProblemFile) match {
      case ParseSuccess(m) =>
        Try {
          Problem(m)
        } match {
          case Success(pb) => pb
          case Failure(NonFatal(e)) =>
            e.printStackTrace()
            sys.error("Error while converting the ANML model: " + e.getLocalizedMessage)
          case Failure(e) => throw e
        }
      case fail: GenFailure => throw new RuntimeException(fail.format)
    }
  }

  def apply(anml: String): Problem = {
    Parser.parse(anml) match {
      case ParseSuccess(m) =>
        Try {
          Problem(m)
        } match {
          case Success(pb) => pb
          case Failure(NonFatal(e)) =>
            e.printStackTrace()
            sys.error("Error while converting the ANML model: " + e.getLocalizedMessage)
          case Failure(e) => throw e
        }
      case fail: GenFailure => throw new RuntimeException(fail.format)
    }
  }
}
