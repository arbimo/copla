package copla.lang.parsing.anml

import copla.lang.model._
import fastparse.core.Parsed.{Failure, Success}
import fastparse.parsers.Transformers.Filtered

object Parser {

  import fastparse.WhitespaceApi
  val whiteChars = " \r\n\t"

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    val white = CharsWhileIn(whiteChars)
    NoTrace(white.rep)
  }
  import fastparse.noApi._
  import White._

  val word: Parser[String] = {
    import fastparse.all._ // override sequence composition to ignore white spaces
    (CharIn(('a' to 'z') ++ ('A' to 'Z')) ~
      CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), min = 0)).!.opaque("word")
  }
  val int: Parser[Int] = CharsWhileIn('0' to '9').!.map(_.toInt).opaque("int")

  val typeKW: Parser[Unit] = word.filter(_ == "type").map(x => {}).opaque("type")
  val instanceKW           = word.filter(_ == "instance").map(_ => {}).opaque("instance")
  val fluentKW             = word.filter(_ == "fluent").map(_ => {}).opaque("instance")
  val timepointKW          = word.filter(_ == "timepoint").map(_ => {}).opaque("instance")
  val durationKW           = word.filter(_ == "duration").map(_ => {}).opaque("duration")
  val keywords             = Set("type", "instance", "action", "duration", "fluent", "predicate", "timepoint")
  val reservedTypeNames    = Set()
  val nonIdent             = keywords ++ reservedTypeNames

  val ident                    = word.opaque("ident").filter(f(!nonIdent.contains(_), "not-reserved"))
  val typeName: Parser[String] = word.filter(!keywords.contains(_)).opaque("type-name")
  val variableName             = ident.opaque("variable-name")

  /** Simple wrapper to attach a toString method to a function */
  private[this] def f[T, V](f: T => V, str: String) = new Function[T, V] {
    def apply(v: T)       = f(v)
    override def toString = str
  }

  def freeIdent(c: Ctx) =
    ident.filter(
      f(name => c.elems.collect { case d: Declaration[_] => d }.forall(name != _.id.name),
        "unused"))

  def declaredType(c: Model): Parser[Type] =
    typeName
      .filter(f(c.findType(_).isDefined, "declared"))
      .map(c.findType(_).get)

  def typeDeclaration(c: Model): Parser[TypeDeclaration] =
    (typeKW ~/
      freeIdent(c)
      ~ ("<" ~/ declaredType(c).!).asInstanceOf[Parser[Type]].?
      ~ ";")
      .map { case (name, parentOpt) => Type(c.id(name), parentOpt) }
      .map(TypeDeclaration(_))

  /** Parser for instance declaration.
    * "instance Type id1, id2, id3;" */
  def instancesDeclaration(m: Model): Parser[Seq[InstanceDeclaration]] = {

    /** Parses a sequences of yet unused *distinct* identifiers. */
    def distinctFreeIdents(m: Model,
                           previous: Seq[String],
                           sep: String,
                           term: String): Parser[Seq[String]] =
      Pass ~ freeIdent(m)
        .filter(f(!previous.contains(_), "not-in-same-instance-list"))
        .flatMap(name =>
          Pass ~ sep ~/ distinctFreeIdents(m, previous :+ name, sep, term)
            | Pass ~ term ~ PassWith(previous :+ name))

    (instanceKW ~/ declaredType(m) ~/ distinctFreeIdents(m, Nil, ",", ";"))
      .map { case (typ, instanceNames) => instanceNames.map(name => Instance(m.id(name), typ)) }
  }.map(instances => instances.map(InstanceDeclaration(_)))

  protected def arg(m: Model): Parser[(String, Type)] =
    (declaredType(m) ~ ident)
      .map { case (typ, argName) => (argName, typ) }

  /** A list of at least one argument formatted as "Type1 arg, Type2 arg2" */
  protected def distinctArgSeq(
      m: Model,
      sep: String,
      previous: Seq[(String, Type)] = Seq()): Parser[Seq[(String, Type)]] =
    Pass ~ arg(m)
      .filter(f(a => !previous.exists(_._1 == a._1), "not-used-in-current-arg-sequence"))
      .flatMap(a =>
        (Pass ~ sep ~/ distinctArgSeq(m, sep, previous :+ a)) | PassWith(previous :+ a))

  /** Parses a sequence of args necessarily enclosed in parenthesis if non empty
    * Example of valid inputs "", "()", "(Type1 arg1)", "(Type1 arg1, Type2 arg2)"
    */
  protected def args(m: Model): Parser[Seq[(String, Type)]] =
    ("(" ~/
      ((&(word) ~/ distinctArgSeq(m, ",")) | PassWith(Seq()).opaque("no-args")) ~
      ")") | // parenthesis with and without args
      PassWith(Seq()).opaque("no-args") // no args no, parenthesis

  def fluentDeclaration(m: Model): Parser[FluentDeclaration] =
    (fluentKW ~/ declaredType(m) ~ freeIdent(m) ~ args(m) ~ ";")
      .map {
        case (typ, svName, args) =>
          FluentTemplate(m.id(svName), typ, args.map {
            case (name, argType) => Arg(Id(m.scope + svName, name), argType)
          })
      }
      .map(FluentDeclaration(_))

  def timepointDeclaration(c: Ctx): Parser[TimepointDeclaration] =
    timepointKW ~/
      freeIdent(c).map(name => TimepointDeclaration(TPRef(c.id(name)))) ~
      ";"

  protected def definedTP(c: Ctx): Parser[TPRef] =
    ident
      .filter(f(c.findTimepoint(_).isDefined, "declared-timepoint"))
      .map(c.findTimepoint(_).get)

  def timepoint(c: Ctx): Parser[TPRef] = {
    (int ~ "+").flatMap(d => timepoint(c).map(tp => tp + d)) |
      (definedTP(c) ~ (("+" | "-").! ~ int).?).map {
        case (tp, Some(("+", delay))) => tp + delay
        case (tp, Some(("-", delay))) => tp - delay
        case (tp, None)               => tp
        case _                        => sys.error("Buggy parser implementation")
      } |
      int.flatMap(
        i => // integer as a tp defined relatively to the global start, if one exists
          c.root.findTimepoint("start") match {
            case Some(st) => PassWith(st + i)
            case None     => Fail.opaque("fail:no start timepoint in top level scope")
        })
  }.opaque("timepoint")

  def delay(c: Ctx): Parser[Delay] =
    (durationKW ~/ Pass)
      .flatMap(_ =>
        c.findTimepoint("start").flatMap(st => c.findTimepoint("end").map(ed => (st, ed))) match {
          case Some((st, ed)) => PassWith(Delay(st, ed + 1))
          case None           => sys.error("No start/end timepoint"); Fail
      })
      .opaque("duration") |
      (timepoint(c) ~ "-" ~ definedTP(c) ~ (("+" | "-").! ~ int).?).map {
        case (t1, t2, None)           => t1 - t2
        case (t1, t2, Some(("+", i))) => (t1 - t2) + i
        case (t1, t2, Some(("-", i))) => (t1 - t2) - i
        case _                        => sys.error("Buggy parser implementation")
      }

  def temporalConstraint(c: Ctx): Parser[Seq[TBefore]] = {
    (timepoint(c) ~ ("<" | "<=" | ">" | ">=" | "==" | ":=" | "=").! ~/ timepoint(c) ~ ";")
      .map {
        case (t1, "<", t2)                                     => Seq(t1 < t2)
        case (t1, "<=", t2)                                    => Seq(t1 <= t2)
        case (t1, ">", t2)                                     => Seq(t1 > t2)
        case (t1, ">=", t2)                                    => Seq(t1 >= t2)
        case (t1, eq, t2) if Set("=", "==", ":=").contains(eq) => t1 === t2
        case _                                                 => sys.error("Buggy parser implementation")
      } |
      (delay(c) ~ ("<" | "<=" | ">" | ">=" | "==" | ":=" | "=").! ~/ int ~ ";")
        .map {
          case (d, "<", t)                                     => Seq(d < t)
          case (d, "<=", t)                                    => Seq(d <= t)
          case (d, ">", t)                                     => Seq(d > t)
          case (d, ">=", t)                                    => Seq(d >= t)
          case (d, eq, t) if Set("=", "==", ":=").contains(eq) => d === t
          case _                                               => sys.error("Buggy parser implementation")
        }
  }

  def variable(c: Ctx): Parser[Var] =
    ident
      .filter(f(c.findVariable(_).isDefined, "declared-variable"))
      .map(c.findVariable(_).get)

  def fluent(c: Ctx): Parser[FluentTemplate] =
    ident
      .filter(f(c.findFluent(_).isDefined, "declared-fluent"))
      .map(c.findFluent(_).get)

  def timedSymExpr(c: Ctx): Parser[TimedSymExpr] = {
    def varList(c: Ctx,
                expectedTypes: Seq[Type],
                sep: String,
                previous: Seq[Var] = Seq()): Parser[Seq[Var]] = {
      if (expectedTypes.isEmpty) {
        PassWith(previous)
      } else {
        variable(c)
          .filter(f(_.typ.isSubtypeOf(expectedTypes.head), "has-expected-type"))
          .flatMap(v =>
            if(expectedTypes.tail.isEmpty)
              PassWith(previous :+ v)
            else
              Pass ~ sep ~/ varList(c, expectedTypes.tail, sep, previous :+ v)
          )
      }
    }
    (fluent(c) ~/ Pass).flatMap(f =>
      f.params.map(param => param.typ) match {
        case Seq() => (("(" ~/ ")") | Pass) ~ PassWith(Fluent(f, Seq()))
        case paramTypes =>
          "(" ~/ varList(c, paramTypes, ",").map(args => Fluent(f, args)) ~ ")" ~/ Pass
    })
  }

  def elem(m: Model): Parser[Seq[ModuleElem]] =
    typeDeclaration(m).map(Seq(_)) |
      instancesDeclaration(m) |
      fluentDeclaration(m).map(Seq(_)) |
      timepointDeclaration(m).map(Seq(_)) |
      temporalConstraint(m)

  def anmlParser(mod: Model): Parser[Model] =
    End ~ PassWith(mod) |
      (Pass ~ elem(mod) ~ Pass).flatMap(elem =>
        mod ++ elem match {
          case Some(extended) => anmlParser(extended)
          case None           => Fail
      })

  def parse(input: String) = {
    anmlParser(baseAnmlModel).parse(input)
  }

  val baseAnmlModel = anmlParser(Model()).parse("""
      |type boolean;
      |instance boolean true, false;
      |timepoint start;
      |timepoint end;
    """.stripMargin) match {
    case Success(m, _) => m
    case x             => sys.error(s"Could not build the base anml model: $x")
  }

}
