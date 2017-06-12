package copla.lang.parsing.anml

import copla.lang.model._
import fastparse.core.Parsed.{Failure, Success}

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
      CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), min = 0)).!
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

  val ident                    = word.filter(!nonIdent.contains(_)).opaque("identifier")
  val typeName: Parser[String] = word.filter(!keywords.contains(_)).opaque("type-name")
  val variableName             = ident.opaque("variable-name")

  def freeIdent(c: Ctx) =
    ident
      .filter(!keywords.contains(_))
      .filter(name => c.elems.collect { case d: Declaration[_] => d }.forall(name != _.id.name))
      .opaque("free-ident")

  def declaredType(c: Model): Parser[Type] =
    typeName
      .flatMap(c.findType(_) match {
        case Some(t) => PassWith(t)
        case None    => Fail
      })
      .opaque("previously-declared-type")

  def typeDeclaration(c: Model): Parser[TypeDeclaration] =
    (typeKW ~/
      freeIdent(c)
        .opaque("previously-undefined-type")
        .!
      ~ ("<" ~/ declaredType(c).!).asInstanceOf[Parser[Type]].?
      ~ ";")
      .map { case (name, parentOpt) => Type(c.id(name), parentOpt) }
      .map(TypeDeclaration(_))
      .opaque("type-declaration")

  /** Parser for instance declaration.
    * "instance Type id1, id2, id3;" */
  def instancesDeclaration(m: Model): Parser[Seq[InstanceDeclaration]] = {

    /** Parses a sequences of yet unused *distinct* identifiers. */
    def distinctFreeIdents(m: Model,
                           previous: Seq[String],
                           sep: String,
                           term: String): Parser[Seq[String]] =
      Pass ~ freeIdent(m)
        .filter(!previous.contains(_))
        .opaque("free-identifier")
        .flatMap(name =>
          Pass ~ sep ~/ distinctFreeIdents(m, previous :+ name, sep, term)
            | Pass ~ term ~ PassWith(previous :+ name))

    (instanceKW ~/ declaredType(m) ~/ distinctFreeIdents(m, Nil, ",", ";"))
      .map { case (typ, instanceNames) => instanceNames.map(name => Instance(m.id(name), typ)) }
  }.map(instances => instances.map(InstanceDeclaration(_)))
    .opaque("instance declaration")

  protected def arg(m: Model): Parser[(String, Type)] =
    (declaredType(m) ~ ident.filter(!keywords.contains(_)))
      .opaque("argument-declaration")
      .map { case (typ, argName) => (argName, typ) }

  /** A liest of at least one argument formatted as "Type1 arg, Type2 arg2" */
  protected def distinctArgSeq(
      m: Model,
      sep: String,
      previous: Seq[(String, Type)] = Seq()): Parser[Seq[(String, Type)]] =
    Pass ~ arg(m)
      .filter(a => !previous.exists(_._1 == a._1)) // disallow args with same name
      .opaque("args")
      .flatMap(a =>
        (Pass ~ sep ~/ distinctArgSeq(m, sep, previous :+ a)) | PassWith(previous :+ a))

  /** Parses a sequence of args necessarily enclosed in parenthesis if non empty
    * Example of valid inputs "", "()", "(Type1 arg1)", "(Type1 arg1, Type2 arg2)"
    */
  protected def args(m: Model): Parser[Seq[(String, Type)]] =
    ("(" ~ (distinctArgSeq(m, ",") | PassWith(Seq())
      .opaque("no-arg")) ~ ")") | // parenthesis with and without args
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
      .map(c.findTimepoint)
      .flatMap {
        case Some(tp) => PassWith(tp)
        case None     => Fail
      }
      .opaque("declared-timepoint")

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
    (timepoint(c) ~ ("<" | "<=" | ">" | ">=" | "==" | ":=" | "=").! ~ timepoint(c) ~ ";")
      .map {
        case (t1, "<", t2)                                     => Seq(t1 < t2)
        case (t1, "<=", t2)                                    => Seq(t1 <= t2)
        case (t1, ">", t2)                                     => Seq(t1 > t2)
        case (t1, ">=", t2)                                    => Seq(t1 >= t2)
        case (t1, eq, t2) if Set("=", "==", ":=").contains(eq) => t1 === t2
        case _                                                 => sys.error("Buggy parser implementation")
      } |
      (delay(c) ~ ("<" | "<=" | ">" | ">=" | "==" | ":=" | "=").! ~ int ~ ";")
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
    ident.flatMap(name =>
      c.findVariable(name) match {
        case Some(v) => PassWith(v)
        case None    => println(c.elems.mkString("\n")); Fail.opaque(s"fail:no variable $name")
    })

  def fluent(c: Ctx): Parser[FluentTemplate] =
    ident.flatMap(name =>
      c.findFluent(name) match {
        case Some(v) => PassWith(v)
        case None    => Fail.opaque(s"fail:fluent not found: $name")
    })

  def timedSymExpr(c: Ctx): Parser[TimedSymExpr] = {
    def varList(c: Ctx,
                expectedTypes: Seq[Type],
                sep: String,
                previous: Seq[Var] = Seq()): Parser[Seq[Var]] = {
      if (expectedTypes.isEmpty) {
        PassWith(previous)
      } else {
        variable(c).flatMap(v => {
          if (v.typ.isSubtypeOf(expectedTypes.head)) {
            if (expectedTypes.tail.isEmpty)
              PassWith(previous :+ v)
            else
              Pass ~ sep ~/ varList(c, expectedTypes.tail, sep, previous :+ v)
          } else {
            Fail.opaque(s"fail:wrong type for var $v (${v.typ}), expecting ${expectedTypes.head}")
          }
        })
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
