package copla.lang.parsing.anml

import copla.lang.model._
import fastparse.core.Parsed.{Failure, Success}

abstract class AnmlParser(val initialContext: Ctx) {
  private[this] var updatableContext = initialContext
  def updateContext(newContext: Ctx) {
    updatableContext = newContext
  }

  def currentContext = updatableContext
  def elems          = currentContext.elems
  def scope          = currentContext.scope
  def parent         = currentContext.parent
  def root           = currentContext.root

  def id(name: String): Id                             = Id(scope, name)
  def findVariable(name: String): Option[Var]          = currentContext.findVariable(name)
  def findTimepoint(name: String): Option[TPRef]       = currentContext.findTimepoint(name)
  def findType(name: String): Option[Type]             = currentContext.findType(name)
  def findFluent(name: String): Option[FluentTemplate] = currentContext.findFluent(name)

  val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    val white = CharsWhileIn(" \r\n\t")
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

  val typeKW            = word.filter(_ == "type").map(x => {}).opaque("type")
  val withKW            = word.filter(_ == "with").map(x => {}).opaque("with")
  val instanceKW        = word.filter(_ == "instance").map(_ => {}).opaque("instance")
  val fluentKW          = word.filter(_ == "fluent").map(_ => {}).opaque("instance")
  val timepointKW       = word.filter(_ == "timepoint").map(_ => {}).opaque("instance")
  val durationKW        = word.filter(_ == "duration").map(_ => {}).opaque("duration")
  val keywords          = Set("type", "instance", "action", "duration", "fluent", "predicate", "timepoint")
  val reservedTypeNames = Set()
  val nonIdent          = keywords ++ reservedTypeNames

  val simpleIdent              = word.opaque("ident").filter(f(!nonIdent.contains(_), "not-reserved"))
  val ident: Parser[String]    = simpleIdent.rep(min = 1, sep = ".").!
  val typeName: Parser[String] = word.filter(!keywords.contains(_)).opaque("type-name")
  val variableName             = ident.opaque("variable-name")

  /** Simple wrapper to attach a toString method to a function */
  protected def f[T, V](f: T => V, str: String) = new Function[T, V] {
    def apply(v: T)       = f(v)
    override def toString = str
  }

  val freeIdent =
    simpleIdent
      .filter(
        f(name => elems.collect { case d: Declaration[_] => d }.forall(name != _.id.name),
          "unused"))

  val declaredType: Parser[Type] =
    typeName
      .filter(f(findType(_).isDefined, "declared"))
      .map(findType(_).get)

  val timepointDeclaration: Parser[TimepointDeclaration] =
    timepointKW ~/
      freeIdent.map(name => TimepointDeclaration(TPRef(id(name)))) ~
      ";"

  protected val definedTP: Parser[TPRef] =
    ident
      .filter(f(findTimepoint(_).isDefined, "declared-timepoint"))
      .map(findTimepoint(_).get)

  val timepoint: Parser[TPRef] = {
    (int ~ "+").flatMap(d => timepoint.map(tp => tp + d)) |
      (definedTP ~ (("+" | "-").! ~ int).?).map {
        case (tp, Some(("+", delay))) => tp + delay
        case (tp, Some(("-", delay))) => tp - delay
        case (tp, None)               => tp
        case _                        => sys.error("Buggy parser implementation")
      } |
      int.flatMap(
        i => // integer as a tp defined relatively to the global start, if one exists
          root.findTimepoint("start") match {
            case Some(st) => PassWith(st + i)
            case None     => Fail.opaque("fail:no start timepoint in top level scope")
        })
  }.opaque("timepoint")

  val delay: Parser[Delay] =
    (durationKW ~/ Pass)
      .flatMap(_ =>
        findTimepoint("start").flatMap(st => findTimepoint("end").map(ed => (st, ed))) match {
          case Some((st, ed)) => PassWith(Delay(st, ed + 1))
          case None           => sys.error("No start/end timepoint"); Fail
      })
      .opaque("duration") |
      (timepoint ~ "-" ~ definedTP ~ (("+" | "-").! ~ int).?).map {
        case (t1, t2, None)           => t1 - t2
        case (t1, t2, Some(("+", i))) => (t1 - t2) + i
        case (t1, t2, Some(("-", i))) => (t1 - t2) - i
        case _                        => sys.error("Buggy parser implementation")
      }

  val temporalConstraint: Parser[Seq[TBefore]] = {
    (timepoint ~ ("<" | "<=" | ">" | ">=" | "==" | ":=" | "=").! ~/ timepoint ~ ";")
      .map {
        case (t1, "<", t2)                                     => Seq(t1 < t2)
        case (t1, "<=", t2)                                    => Seq(t1 <= t2)
        case (t1, ">", t2)                                     => Seq(t1 > t2)
        case (t1, ">=", t2)                                    => Seq(t1 >= t2)
        case (t1, eq, t2) if Set("=", "==", ":=").contains(eq) => t1 === t2
        case _                                                 => sys.error("Buggy parser implementation")
      } |
      (delay ~ ("<" | "<=" | ">" | ">=" | "==" | ":=" | "=").! ~/ int ~ ";")
        .map {
          case (d, "<", t)                                     => Seq(d < t)
          case (d, "<=", t)                                    => Seq(d <= t)
          case (d, ">", t)                                     => Seq(d > t)
          case (d, ">=", t)                                    => Seq(d >= t)
          case (d, eq, t) if Set("=", "==", ":=").contains(eq) => d === t
          case _                                               => sys.error("Buggy parser implementation")
        }
  }

  val variable: Parser[Var] =
    ident
      .filter(f(findVariable(_).isDefined, "declared-variable"))
      .map(findVariable(_).get)

  val fluent: Parser[FluentTemplate] =
    ident
      .filter(f(findFluent(_).isDefined, "declared-fluent"))
      .map(findFluent(_).get)

  val timedSymExpr: Parser[TimedSymExpr] = {
    def varList(expectedTypes: Seq[Type],
                sep: String,
                previous: Seq[Var] = Seq()): Parser[Seq[Var]] = {
      if (expectedTypes.isEmpty) {
        PassWith(previous)
      } else {
        variable
          .filter(f(_.typ.isSubtypeOf(expectedTypes.head), "has-expected-type"))
          .flatMap(
            v =>
              if (expectedTypes.tail.isEmpty)
                PassWith(previous :+ v)
              else
                Pass ~ sep ~/ varList(expectedTypes.tail, sep, previous :+ v))
      }
    }
    (fluent ~/ Pass).flatMap(f =>
      f.params.map(param => param.typ) match {
        case Seq() => (("(" ~/ ")") | Pass) ~ PassWith(Fluent(f, Seq()))
        case paramTypes =>
          "(" ~/ varList(paramTypes, ",").map(args => Fluent(f, args)) ~ ")" ~/ Pass
    })
  }

  val staticSymExpr: Parser[StaticSymExpr] =
    variable

  val symExpr: Parser[SymExpr] =
    timedSymExpr

  val interval: Parser[Interval] =
    ("[" ~/ timepoint ~ "," ~/ timepoint ~ "]").map {
      case (tp1, tp2) => Interval(tp1, tp2)
    }

  val timedAssertion: Parser[TimedAssertion] = {

    /** Read an identifier or construct a default one otherwise */
    val assertionId: Parser[String] =
      (freeIdent ~ ":" ~/ Pass) | PassWith(defaultId())

    assertionId ~
      (timedSymExpr ~ "==" ~/ staticSymExpr).filter(f({
        case (left, right) => left.typ.isSubtypeOf(right.typ) || right.typ.isSubtypeOf(left.typ)
      }, "equality-between-compatible-types"))
  }.map {
    case (id, (left, right)) => TimedEqualAssertion(left, right, Some(currentContext), id)
  }

  val temporallyQualifiedAssertion: Parser[TemporallyQualifiedAssertion] = {
    (interval ~/ timedAssertion ~ ";")
      .map { case (it, assertion) => TemporallyQualifiedAssertion(it, assertion) }
  }

}

class AnmlModuleParser(val initialModel: Model) extends AnmlParser(initialModel) {
  import fastparse.noApi._
  import White._

  val typeDeclaration: Parser[TypeDeclaration] =
    (typeKW ~/
      freeIdent
      ~ ("<" ~/ declaredType.!).asInstanceOf[Parser[Type]].?
      ~ ";")
      .map { case (name, parentOpt) => TypeDeclaration(Type(id(name), parentOpt)) }

  /** Parser for instance declaration.
    * "instance Type id1, id2, id3;" */
  val instancesDeclaration: Parser[Seq[InstanceDeclaration]] = {

    /** Parses a sequences of yet unused *distinct* identifiers. */
    def distinctFreeIdents(previous: Seq[String], sep: String, term: String): Parser[Seq[String]] =
      Pass ~ freeIdent
        .filter(f(!previous.contains(_), "not-in-same-instance-list"))
        .flatMap(name =>
          Pass ~ sep ~/ distinctFreeIdents(previous :+ name, sep, term)
            | Pass ~ term ~ PassWith(previous :+ name))

    (instanceKW ~/ declaredType ~/ distinctFreeIdents(Nil, ",", ";"))
      .map { case (typ, instanceNames) => instanceNames.map(name => Instance(id(name), typ)) }
  }.map(instances => instances.map(InstanceDeclaration(_)))

  val fluentDeclaration: Parser[FluentDeclaration] = {
    val arg: Parser[(String, Type)] =
      (declaredType ~ ident)
        .map { case (typ, argName) => (argName, typ) }

    /** A list of at least one argument formatted as "Type1 arg, Type2 arg2" */
    def distinctArgSeq(sep: String,
                       previous: Seq[(String, Type)] = Seq()): Parser[Seq[(String, Type)]] =
      Pass ~ arg
        .filter(f(a => !previous.exists(_._1 == a._1), "not-used-in-current-arg-sequence"))
        .flatMap(a => (Pass ~ sep ~/ distinctArgSeq(sep, previous :+ a)) | PassWith(previous :+ a))

    /** Parses a sequence of args necessarily enclosed in parenthesis if non empty
      * Example of valid inputs "", "()", "(Type1 arg1)", "(Type1 arg1, Type2 arg2)"
      */
    val args: Parser[Seq[(String, Type)]] =
      ("(" ~/
        ((&(word) ~/ distinctArgSeq(",")) | PassWith(Seq()).opaque("no-args")) ~
        ")") | // parenthesis with and without args
        PassWith(Seq()).opaque("no-args") // no args no, parenthesis

    (fluentKW ~/ declaredType ~ freeIdent ~ args ~ ";")
      .map {
        case (typ, svName, args) =>
          FluentTemplate(id(svName), typ, args.map {
            case (name, argType) => Arg(Id(scope + svName, name), argType)
          })
      }
      .map(FluentDeclaration(_))
  }

  val elem: Parser[Seq[ModuleElem]] =
    typeDeclaration.map(Seq(_)) |
      instancesDeclaration |
      fluentDeclaration.map(Seq(_)) |
      timepointDeclaration.map(Seq(_)) |
      temporalConstraint |
      temporallyQualifiedAssertion.map(Seq(_))

  private[this] def currentModel: Model = currentContext match {
    case m: Model => m
    case x        => sys.error("Current context is not a model")
  }

  private[this] def anmlParser: Parser[Model] =
    End ~ PassWith(currentModel) |
      (Pass ~ elem ~ Pass).flatMap(elem =>
        currentModel ++ elem match {
          case Some(extended) =>
            updateContext(extended) // change state
            anmlParser
          case None => Fail.opaque("fail: parsed elem does not fit into the previous model")
      })

  def parse(input: String) = {
    updateContext(initialModel)
    anmlParser.parse(input)
  }
}

object Parser {

  /** ANML model with default definitions already added */
  val baseAnmlModel: Model = new AnmlModuleParser(Model()).parse("""
      |type boolean;
      |instance boolean true, false;
      |timepoint start;
      |timepoint end;
    """.stripMargin) match {
    case Success(m, _) => m
    case x             => sys.error(s"Could not build the base anml model: $x")
  }
  val parser = new AnmlModuleParser(baseAnmlModel)

  def parse(input: String) = {
    parser.parse(input)
  }
}
