package copla.constraints.meta.updates

import cats.Monad
import org.scalatest.FunSuite

import scala.annotation.tailrec

class UpdateTest extends FunSuite {

  def div(num: Int, denum: Int): Update[Int] = {
    check {
      denum match {
        case 0 => fatal("division by zero")
        case _ => consistent(num / denum)
      }
    }
  }

  def effect(str: String): Update[Status] = {
    check { println(str) }
  }

  import cats.implicits._

  test("test for updates") {
    val ints = Seq(1, 2, 0)

    val res = for {
      i  <- consistent(10)
      j  <- consistent(3)
      _  <- effect(j.toString)
      d  <- div(i, j)
      d2 <- div(d, 9)
    } yield d2
    assert(res.ok)

    val res2 = for {
      i  <- consistent(10)
      j  <- consistent(0)
      _  <- effect(j.toString)
      d  <- div(i, j)
      d2 <- div(d, 9)
    } yield d2

    //  Monad[Update].

    assert(foreach(Seq(1, 2, 3)) {
      div(10, _)
    }.ok)
    assert(!foreach(Seq(1, 2, 0, 10, 1, 2, 8)) {
      div(10, _)
    }.ok)
  }

}
