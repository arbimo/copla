package copla.lang.parsing.anml

import copla.lang.model._
import fastparse.core.Parsed.{Failure, Success}

abstract class AnmlParser(val initialContext: Ctx) {
  private[this] var updatableContext = initialContext
  protected def updateContext(newContext: Ctx) {
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
  val ident: Parser[String]    = simpleIdent.rep(min = 1, sep = ".").!.opaque("possibly-nested-ident")
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

  /** Parses a fluent in the object oriented notation.
    * "x.f" where x is a variable of type T and f is a fluent declared in type T or in a supertype of T.
    * Returns the fluent T.f and x which is to be the first argument of T.f */
  val partiallyAppliedFluent: Parser[(FluentTemplate, Var)] = {

    /** Retrieves a fluent template declared the the given type or one of its super types.*/
    def findInTypeFluent(typ: Type, fluentName: String): Option[FluentTemplate] =
      findFluent(typ.id.name + "." + fluentName).orElse(typ.parent.flatMap(p =>
        findInTypeFluent(p, fluentName)))

    ident
      .map(str => str.split("\\.").toList)
      // split into first idents (variable) and last (fluent name)
      .map(idents => (idents.dropRight(1), idents.last))
      // only keep if first idents represent a valid variable
      .map(tup => (findVariable(tup._1.mkString(".")), tup._2))
      .filter(f(_._1.isDefined, "declared-variable"))
      .map(tup => (tup._1.get, tup._2))
      // keep if we can find the fluent in the type of the variable
      .filter(f({ case (v, fluentName) => findInTypeFluent(v.typ, fluentName).isDefined },
                s"fluent-available-for-this-variable-type"))
      // return the fluent and the variable
      .map { case (v, fluentName) => (findInTypeFluent(v.typ, fluentName).get, v) }
  }

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
    }) |
      (partiallyAppliedFluent ~/ Pass).flatMap {
        case (f, firstArg) =>
          f.params.map(param => param.typ) match {
            case Seq() => (("(" ~/ ")") | Pass) ~ PassWith(Fluent(f, Seq(firstArg)))
            case paramTypes =>
              "(" ~/ varList(paramTypes.tail, ",")
                .map(args => Fluent(f, firstArg +: args)) ~ ")" ~/ Pass
          }
      }
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

/** Second phase parser that extracts all ANML elements expects types that should
  *  be already present in the initial model. */
class AnmlModuleParser(val initialModel: Model) extends AnmlParser(initialModel) {
  import fastparse.noApi._
  import White._

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

  /** Extract the functions declared in a type. This consumes the whole type declaration.
    * Note that the type should be already present in the module.
    * Typically consumed:
    *   "type B < A with { fluent f(C c); };" */
  val inTypeFunctionDeclaration: Parser[Seq[FluentDeclaration]] =
    (typeKW ~/
      declaredType
      ~ ("<" ~/ declaredType.!).asInstanceOf[Parser[Type]].?
      ~ (withKW ~/ "{" ~/ fluentDeclaration.rep ~ "}").?
      ~ ";")
      .map {
        case (t, _, None) => Seq()
        case (t, _, Some(fluentDeclarations)) =>
          fluentDeclarations.map(fd => {
            val id            = Id(t.asScope, fd.fluent.id.name)
            val functionScope = t.asScope + id.name
            val selfArg       = Arg(Id(functionScope, "self"), t)
            val params = selfArg +: fd.fluent.params.map(arg =>
              Arg(Id(functionScope, arg.id.name), arg.typ))
            FluentDeclaration(FluentTemplate(id, fd.fluent.typ, params))
          })
      }

  val elem: Parser[Seq[ModuleElem]] =
    inTypeFunctionDeclaration |
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

/** First phase parser used to extract all type declarations from a given ANML string. */
class AnmlTypeParser(val initialModel: Model) extends AnmlParser(initialModel) {
  import fastparse.noApi._
  import White._

  val nonTypeToken = (word | int | CharIn("{}[]();=:<>-+.,")).!.filter(_ != "type")
  val typeDeclaration = (typeKW ~/ freeIdent ~ ("<" ~ declaredType).? ~ (";" | withKW)).map {
    case (name, parentOpt) => TypeDeclaration(Type(id(name), parentOpt))
  }

  private[this] def currentModel: Model = currentContext match {
    case m: Model => m
    case x        => sys.error("Current context is not a model")
  }

  private[this] def parser: Parser[Model] =
    End ~ PassWith(currentModel) |
      (Pass ~ nonTypeToken ~/ Pass).flatMap(_ => parser) |
      (Pass ~ typeDeclaration ~ Pass).flatMap(typeDecl =>
        currentModel + typeDecl match {
          case Some(extendedModel) =>
            updateContext(extendedModel)
            parser
          case None => Fail
      })

  def parse(input: String) = {
    updateContext(initialModel)
    parser.parse(input)
  }
}

object Parser {

  val anmlHeader =
    """|type boolean;
       |instance boolean true, false;
       |timepoint start;
       |timepoint end;
    """.stripMargin

  /** ANML model with default definitions already added */
  val baseAnmlModel: Model = new AnmlTypeParser(Model()).parse(anmlHeader) match {
    case Success(m, _) =>
      new AnmlModuleParser(m).parse(anmlHeader) match {
        case Success(m, _) => m
        case x             => sys.error(s"could not parse the ANML header: $x")
      }
    case x => sys.error(s"Could not parse types the base anml model: $x")
  }

  def parse(input: String) = {
    new AnmlTypeParser(baseAnmlModel).parse(input) match {
      case Success(m, _) =>
        new AnmlModuleParser(m).parse(input)
      case x => x
    }
  }
}
