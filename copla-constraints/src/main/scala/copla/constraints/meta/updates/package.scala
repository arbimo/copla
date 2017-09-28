package copla.constraints.meta

import java.io.File

import shapeless.<:!<

import scala.util.control.NonFatal
import cats._

import scala.annotation.tailrec
import scala.reflect.internal.annotations

package object updates {

  sealed trait Update[+A] {
    def ok: Boolean
    def map[B](f: A => B): Update[B]
    def flatMap[B](f: A => Update[B]): Update[B]

    def ==>[B](f: A => Update[B]): Update[B] = flatMap(f)
  }

  implicit val updateMonad = new Monad[Update] {
    override def flatMap[A, B](fa: Update[A])(f: (A) => Update[B]): Update[B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: (A) => Update[Either[A, B]]): Update[B] = f(a) match {
      case x: Failure => x
      case Consistent(Right(b)) => Consistent(b)
      case Consistent(Left(nextA)) => tailRecM(nextA)(f)
    }

    override def pure[A](x: A): Update[A] = consistent(x)
  }

  /** Update that does not return any value. */
  type Up = Update[Status]

  /** Simple trait to represent Update with no return values. */
  sealed trait Status
  object Ok extends Status

  final case class Consistent[+A](value: A) extends Update[A] {
    override def ok: Boolean = true
    override def map[B](f: (A) => B): Update[B] = Consistent(f(value))
    override def flatMap[B](f: (A) => Update[B]): Update[B] = f(value)
  }

  sealed trait Failure extends Update[Nothing]

  final case class XInconsistent(msg: String) extends Failure {
    override def ok: Boolean = false
    override def map[B](f: (Nothing) => B): XInconsistent = this
    override def flatMap[B](f: (Nothing) => Update[B]): XInconsistent = this
  }
  case class XFatalError(msg: String, ex: Option[Throwable]) extends Failure {
    override def ok: Boolean = false
    override def map[B](f: (Nothing) => B): XFatalError = this
    override def flatMap[B](f: (Nothing) => Update[B]): XFatalError = this
  }

  private val consistentOk: Consistent[Status] = Consistent(Ok)
  def consistent: Consistent[Status] = consistentOk
  def consistent[T](value: T) = Consistent(value)
  def inconsistent(msg: String) = XInconsistent(msg)
  def fatal(msg: String, ex: Throwable): XFatalError = {
    Console.err.println(s"Error: $msg -- ${ex.getLocalizedMessage}")
    XFatalError(msg, Option(ex))
  }
  def fatal(msg: String)(implicit file: sourcecode.File, line: sourcecode.Line): XFatalError = {
    Console.err.println(s"Error: $msg (${new File(file.value).getName}:${line.value})")
    XFatalError(msg, None)
  }

  def check[T](f: => T)(implicit ev: T <:!< Update[Any]): Update[Status] = {
    try {
      f
      consistent
    } catch {
      case NonFatal(e) => fatal("check failed", e)
    }
  }

  def check[T](f: => Update[T]): Update[T] = {
    try {
      f
    } catch {
      case NonFatal(e) => fatal("check failed", e)
    }
  }


  import cats.implicits._
  def foreach[A,B](xs: Iterable[A])(f: A => Update[B]): Update[Status] = {
    var up: Update[Status] = consistent
    val it = xs.iterator
    while(it.hasNext && up.ok) {
      println("a")
      up = up << f(it.next())
    }
    up
  }

}
