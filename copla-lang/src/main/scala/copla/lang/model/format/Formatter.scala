package copla.lang.model.format

import shapeless._

/** Type class for formatting. */
trait Formatter[-T] {
  def format(t: T, indent: Int = 0): String
}

trait LowPriorityFormatter {

  private val anyFormatter: Formatter[Any] =
    (t: Any, indent: Int) => " "*indent + t.toString

  /** Default instance simply invoke the toString method. */
  implicit def fmtDefault[T](implicit ev: T <:!< HList): Formatter[T] = anyFormatter
}

object Formatter extends LowPriorityFormatter {
  import copla.lang.model.core._

  def apply[T](implicit fmt: Formatter[T]): Formatter[T] = fmt

  implicit def fAction(implicit ev: Formatter[Statement]) = new Formatter[ActionTemplate] {
    override def format(t: ActionTemplate, indent: Int): String = {
      val sb = new StringBuilder()
      sb ++= "\n" + " "*indent + t.toString + ":"
      for (s <- t.content)
        sb ++= "\n" + ev.format(s, indent + 2)
      sb.toString
    }
  }

  implicit val cnilFormatter: Formatter[CNil] = (_, _) => sys.error("")
  implicit def coprodFormatter[H, T <: Coproduct](implicit hFmt: Lazy[Formatter[H]],
                                                  tFmt: Formatter[T]): Formatter[H :+: T] =
    (t: H :+: T, indent: Int) =>
      t match {
        case Inl(x) => hFmt.value.format(x, indent)
        case Inr(x) => tFmt.format(x, indent)
    }

  implicit def genFormatter[T, Expr](implicit gen: Generic.Aux[T, Expr], fmt: Formatter[Expr]): Formatter[T] =
    (t: T, indent: Int) => fmt.format(gen.to(t), indent)

  implicit def collFormatter[T, Coll[_]](implicit fmt: Formatter[T], ev: Coll[T] <:< Traversable[T]): Formatter[Coll[T]] =
    (t: Coll[T], indent: Int) => t.map(fmt.format(_, indent)).mkString("\n")
}