package copla

import java.io.File

import copla.lang.model.core.CoreModel
import copla.lang.model.{core, full}
import copla.lang.parsing.anml

package object lang {

  sealed trait Result[+T] {
    def map[B](f: T => B): Result[B]
  }
  sealed trait Failure extends Result[Nothing] {
    override def map[B](f: Nothing => B): Result[B] = this
  }

  case class ParseError(failure: anml.GenFailure) extends Failure

  case class Crash(msg: String, throwable: Option[Throwable]) extends Failure

  case class Success[T](result: T) extends Result[T] {
    override def map[B](f: (T) => B): Success[B] = Success(f(result))
  }


  def parse(anml: String): Result[core.CoreModel] =
    parseToFull(anml).map(_.asCore())

  def parse(anml: File): Result[CoreModel] =
    parseToFull(anml).map(_.asCore())

  def parseToFull(anmlString: String): Result[full.Model] = {
    anml.Parser.parse(anmlString) match {
      case anml.ParseSuccess(model) =>
        Success(model)
      case x: anml.GenFailure =>
        ParseError(x)
    }
  }

  def parseToFull(anmlFile: File): Result[full.Model] = {
    anml.Parser.parse(anmlFile) match {
      case anml.ParseSuccess(model) =>
        Success(model)
      case x: anml.GenFailure =>
        ParseError(x)
    }
  }

}
