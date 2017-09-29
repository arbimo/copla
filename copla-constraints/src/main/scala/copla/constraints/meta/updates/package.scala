package copla.constraints.meta

import java.io.File

import shapeless.<:!<

import scala.util.control.NonFatal
import cats._

import scala.annotation.tailrec

package object updates {

  sealed trait UpdateResult[+A] {
    def ok: Boolean
    def map[B](f: A => B): UpdateResult[B]
    def flatMap[B](f: A => UpdateResult[B]): UpdateResult[B]

    final def <<[T](next: => T)(implicit ev: T <:!< UpdateResult[Any]): UpdateResult[A] =
      if(ok) { next; this }
      else this
    final def >>[T](next: => UpdateResult[T]): UpdateResult[T] = flatMap(_ => next)

  }

  implicit val updateMonad = new Monad[UpdateResult] {
    override def flatMap[A, B](fa: UpdateResult[A])(f: (A) => UpdateResult[B]): UpdateResult[B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: (A) => UpdateResult[Either[A, B]]): UpdateResult[B] = f(a) match {
      case x: Failure => x
      case Consistent(Right(b)) => Consistent(b)
      case Consistent(Left(nextA)) => tailRecM(nextA)(f)
    }

    override def pure[A](x: A): UpdateResult[A] = consistent(x)
  }

  /** Update that does not return any value. */
  type Update = UpdateResult[NoReturn]

  /** Simple trait to represent Update with no return values. */
  sealed trait NoReturn
  private object Empty extends NoReturn

  final case class Consistent[+A](value: A) extends UpdateResult[A] {
    override def ok: Boolean = true
    override def map[B](f: (A) => B): UpdateResult[B] = Consistent(f(value))
    override def flatMap[B](f: (A) => UpdateResult[B]): UpdateResult[B] = f(value)
  }

  sealed trait Failure extends UpdateResult[Nothing]

  final case class Inconsistent(msg: String) extends Failure {
    override def ok: Boolean = false
    override def map[B](f: (Nothing) => B): Inconsistent = this
    override def flatMap[B](f: (Nothing) => UpdateResult[B]): Inconsistent = this
  }
  case class FatalError(msg: String, ex: Option[Throwable]) extends Failure {
    override def ok: Boolean = false
    override def map[B](f: (Nothing) => B): FatalError = this
    override def flatMap[B](f: (Nothing) => UpdateResult[B]): FatalError = this
  }

  private val consistentOk: Consistent[NoReturn] = Consistent(Empty)
  def consistent: Consistent[NoReturn] = consistentOk
  def consistent[T](value: T) = Consistent(value)
  def inconsistent(msg: String) = Inconsistent(msg)
  def fatal(msg: String, ex: Throwable): FatalError = {
    Console.err.println(s"Error: $msg -- ${ex.getLocalizedMessage}")
    FatalError(msg, Option(ex))
  }
  def fatal(msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): FatalError = {
    Console.err.println(s"Error: $msg (${new File(file.value).getName}:${line.value})")
    FatalError(msg, None)
  }

  def check[T](f: => T)(implicit ev: T <:!< UpdateResult[Any]): UpdateResult[NoReturn] = {
    try {
      f
      consistent
    } catch {
      case NonFatal(e) => fatal("check failed", e)
    }
  }

  def check[T](f: => UpdateResult[T]): UpdateResult[T] = {
    try {
      f
    } catch {
      case NonFatal(e) => fatal("check failed", e)
    }
  }


  import cats.implicits._
  def foreach[A,B](xs: Iterable[A])(f: A => UpdateResult[B]): UpdateResult[NoReturn] = {
    var up: UpdateResult[NoReturn] = consistent
    val it = xs.iterator
    while(it.hasNext && up.ok) {
      up = up.forEffect(f(it.next()))
    }
    up
  }

}
