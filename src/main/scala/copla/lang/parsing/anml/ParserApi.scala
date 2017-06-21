package copla.lang.parsing.anml

object ParserApi {

  val baseApi = fastparse.noApi
  val whiteApi = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    val white = CharsWhileIn(" \r\n\t")
    NoTrace(white.rep)
  }

  /** Implicit class that monkey patch FastParse's Parsers with convenience methods. */
  object extendedApi {
    import baseApi._
    import whiteApi._

    implicit class ParserWithNamedFilter[T](parser: Parser[T]) {

      /** Identical to `filter` but gives a name to the filter to ease debugging. */
      def namedFilter(predicate: T => Boolean, name: String): Parser[T] =
        parser.filter(new Function[T, Boolean] {
          override def apply(v1: T) = predicate(v1)
          override def toString()   = name
        })
    }

    implicit class ParserWithOptionSuccess[T](parser: Parser[T]) {

      /** Given a parser of T, make it a parser of V that
        * - succeeds with `v` if the transformation returns `Some(v: V)`
        * - fails if the transformation returns `None`.
        *
        * Optionally, the filtering step can be given a name `filterName` that will appear in parse failures.*/
      def optGet[V](trans: T => Option[V], filterName: String): Parser[V] =
        parser
          .map(content => trans(content))
          .namedFilter(optContent => optContent.isDefined, filterName)
          .map {
            case Some(value) => value
            case None        => sys.error("Empty option should have been filtered out.")
          }

      def optGet[V](trans: T => Option[V]): Parser[V] = optGet(trans, trans.toString())
    }

    implicit class ParserWithSilent[T](parser: Parser[T]) {

      /** Make the parser silent. */
      def silent: Parser[Unit] = parser.map(_ => {})
    }
  }

}
