package copla.lang.parsing.anml

import copla.lang.model._

import ParserApi.baseApi._
import ParserApi.baseApi.Parsed.Success
import ParserApi.whiteApi._
import ParserApi.extendedApi._

abstract class AnmlParser(val initialContext: Ctx) {

  /** Denotes the current context of this AnmlParser.
    * It is used by many suparsers to find the variable/fluent/type associated to an identifier.
    * Over the course of Parsing, the current context is likely to change (i.e. `ctx` will point to
    * a new context since [[Ctx]] is immutable. */
  protected var ctx = initialContext
  protected def updateContext(newContext: Ctx) {
    ctx = newContext
  }

  val word: Parser[String] = {
    import fastparse.all._ // override sequence composition to ignore white spaces
    (CharIn(('a' to 'z') ++ ('A' to 'Z') ++ "_") ~
      CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "_", min = 0)).!.opaque("word")
  }
  val int: Parser[Int] = CharsWhileIn('0' to '9').!.map(_.toInt).opaque("int")

  val typeKW      = word.filter(_ == "type").silent.opaque("type")
  val withKW      = word.filter(_ == "with").silent.opaque("with")
  val instanceKW  = word.filter(_ == "instance").silent.opaque("instance")
  val fluentKW    = word.filter(_ == "fluent").silent.opaque("fluent")
  val constantKW  = word.filter(_ == "constant").silent.opaque("constant")
  val timepointKW = word.filter(_ == "timepoint").silent.opaque("instance")
  val actionKW    = word.filter(_ == "action").silent.opaque("action")
  val durationKW  = word.filter(_ == "duration").silent.opaque("duration")
  val keywords =
    Set("type", "instance", "action", "duration", "fluent", "variable", "predicate", "timepoint")
  val reservedTypeNames = Set()
  val nonIdent          = keywords ++ reservedTypeNames

  val simpleIdent              = word.opaque("ident").namedFilter(!nonIdent.contains(_), "not-reserved")
  val ident: Parser[String]    = simpleIdent.rep(min = 1, sep = ".").!.opaque("possibly-nested-ident")
  val typeName: Parser[String] = word.filter(!keywords.contains(_)).opaque("type-name")
  val variableName             = ident.opaque("variable-name")

  val freeIdent =
    simpleIdent
      .namedFilter(id =>
                     ctx.findDeclaration(id) match {
                       case Some(_) => false
                       case None    => true
                   },
                   "unused")

  val declaredType: Parser[Type] =
    typeName.optGet(ctx.findType(_), "declared")

  val timepointDeclaration: Parser[TimepointDeclaration] =
    timepointKW ~/
      freeIdent.map(name => TimepointDeclaration(TPRef(ctx.id(name)))) ~
      ";"

  protected val definedTP: Parser[TPRef] =
    ident.optGet(ctx.findTimepoint(_), "declared-timepoint")

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
          ctx.root.findTimepoint("start") match {
            case Some(st) => PassWith(st + i)
            case None     => Fail.opaque("fail: no start timepoint in top level scope")
        })
  }.opaque("timepoint")

  val delay: Parser[Delay] =
    (durationKW ~/ Pass)
      .flatMap(
        _ =>
          ctx
            .findTimepoint("start")
            .flatMap(st => ctx.findTimepoint("end").map(ed => (st, ed))) match {
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
    ident.optGet(ctx.findVariable(_), "declared-variable")

  val fluent: Parser[FluentTemplate] =
    ident.optGet(ctx.findFluent(_), "declared-fluent")

  val constantFunc: Parser[ConstantTemplate] =
    ident.optGet(ctx.findConstant(_), "declared-fluent")

  /** Parses a fluent in the object oriented notation.
    * "x.f" where x is a variable of type T and f is a fluent declared in type T or in a supertype of T.
    * Returns the fluent T.f and x which is to be the first argument of T.f */
  val partiallyAppliedFunction: Parser[(FunctionTemplate, Var)] = {

    /** Retrieves a fluent template declared the the given type or one of its super types.*/
    def findInTypeFunction(typ: Type, fluentName: String): Option[FunctionTemplate] =
      ctx
        .findFunction(typ.id.name + "." + fluentName)
        .orElse(typ.parent.flatMap(p => findInTypeFunction(p, fluentName)))

    ident
      .map(str => str.split("\\.").toList)
      // split into first idents (variable) and last (fluent name)
      .map(idents => (idents.dropRight(1), idents.last))
      // only keep if first idents represent a valid variable
      .map(tup => (ctx.findVariable(tup._1.mkString(".")), tup._2))
      .namedFilter(_._1.isDefined, "declared-variable")
      .map(tup => (tup._1.get, tup._2))
      // keep if we can find the fluent in the type of the variable
      .namedFilter({ case (v, fluentName) => findInTypeFunction(v.typ, fluentName).isDefined },
                   s"fluent-available-for-this-variable-type")
      // return the fluent and the variable
      .map { case (v, fluentName) => (findInTypeFunction(v.typ, fluentName).get, v) }
  }

  private[this] def varList(expectedTypes: Seq[Type],
                            sep: String,
                            previous: Seq[StaticSymExpr] = Seq()): Parser[Seq[StaticSymExpr]] = {
    if (expectedTypes.isEmpty) {
      PassWith(previous)
    } else {
      staticSymExpr
        .namedFilter(_.typ.isSubtypeOf(expectedTypes.head), "has-expected-type")
        .flatMap(
          v =>
            if (expectedTypes.tail.isEmpty)
              PassWith(previous :+ v)
            else
              Pass ~ sep ~/ varList(expectedTypes.tail, sep, previous :+ v))
    }
  }

  /** Parses a sequence of args necessarily enclosed in parenthesis if non empty
    * Example of valid inputs "", "()", "(Type1 arg1)", "(Type1 arg1, Type2 arg2)"
    */
  protected val argList: Parser[Seq[(String, Type)]] = {
    val arg: Parser[(String, Type)] =
      (declaredType ~ ident)
        .map { case (typ, argName) => (argName, typ) }

    /** A list of at least one argument formatted as "Type1 arg, Type2 arg2" */
    def distinctArgSeq(sep: String,
                       previous: Seq[(String, Type)] = Seq()): Parser[Seq[(String, Type)]] =
      Pass ~ arg
        .namedFilter(a => !previous.exists(_._1 == a._1), "not-used-in-current-arg-sequence")
        .flatMap(a => (Pass ~ sep ~/ distinctArgSeq(sep, previous :+ a)) | PassWith(previous :+ a))

    ("(" ~/
      ((&(word) ~/ distinctArgSeq(",")) | PassWith(Seq()).opaque("no-args")) ~
      ")") | // parenthesis with and without args
      PassWith(Seq()).opaque("no-args") // no args no, parenthesis
  }

  val timedSymExpr: Parser[TimedSymExpr] = {
    val partiallyAppliedFluent = partiallyAppliedFunction
      .namedFilter(_._1.isInstanceOf[FluentTemplate], "is-fluent")
      .map(tup => (tup._1.asInstanceOf[FluentTemplate], tup._2))

    (fluent ~/ Pass).flatMap(f =>
      f.params.map(param => param.typ) match {
        case Seq() => (("(" ~/ ")") | Pass) ~ PassWith(Fluent(f, Seq()))
        case paramTypes =>
          "(" ~/ varList(paramTypes, ",").map(args => Fluent(f, args)) ~ ")" ~/ Pass
    }) |
      (partiallyAppliedFluent ~/ Pass).flatMap {
        case (f, firstArg) =>
          f.params.map(param => param.typ) match {
            case Seq(singleParam) => (("(" ~/ ")") | Pass) ~ PassWith(Fluent(f, Seq(firstArg)))
            case paramTypes =>
              "(" ~/ varList(paramTypes.tail, ",")
                .map(args => Fluent(f, firstArg +: args)) ~ ")" ~/ Pass
          }
      }
  }

  val staticSymExpr: Parser[StaticSymExpr] = {
    val partiallyAppliedConstant = partiallyAppliedFunction
      .namedFilter(_._1.isInstanceOf[ConstantTemplate], "is-constant")
      .map(tup => (tup._1.asInstanceOf[ConstantTemplate], tup._2))

    variable |
      (constantFunc ~/ Pass).flatMap(f =>
        f.params.map(param => param.typ) match {
          case Seq() => (("(" ~/ ")") | Pass) ~ PassWith(Constant(f, Seq()))
          case paramTypes =>
            "(" ~/ varList(paramTypes, ",").map(args => Constant(f, args)) ~ ")" ~/ Pass
      }) |
      (partiallyAppliedConstant ~/ Pass).flatMap {
        case (f, firstArg) =>
          f.params.map(param => param.typ) match {
            case Seq(singleParam) => (("(" ~/ ")") | Pass) ~ PassWith(Constant(f, Seq(firstArg)))
            case paramTypes =>
              "(" ~/ varList(paramTypes.tail, ",")
                .map(args => Constant(f, firstArg +: args)) ~ ")" ~/ Pass
          }
      }
  }

  val symExpr: Parser[SymExpr] =
    timedSymExpr | staticSymExpr

  val interval: Parser[Interval] =
    ("[" ~/
      ((timepoint ~/ ("," ~/ timepoint).?).map {
        case (tp, None)       => (tp, tp) // "[end]" becomes "[end, end]"
        case (tp1, Some(tp2)) => (tp1, tp2)
      } |
        P("all").map(_ => {
          (ctx.findTimepoint("start"), ctx.findTimepoint("end")) match {
            case (Some(st), Some(ed)) => (st, ed)
            case _                    => sys.error("Start and/or end timepoints are not defined.")
          }
        })) ~
      "]").map {
      case (tp1, tp2) => Interval(tp1, tp2)
    }

  val timedAssertion: Parser[TimedAssertion] = {
    // variable that hold the first two parsed token to facilitate type checking logic
    var id: String           = null
    var fluent: TimedSymExpr = null

    def compatibleTypes(t1: Type, t2: Type): Boolean = t1.isSubtypeOf(t2) || t2.isSubtypeOf(t1)

    /** Reads a static symbolic expressions whose type is compatible with the one of the fluent */
    val rightSideExpr: Parser[StaticSymExpr] = staticSymExpr.namedFilter(
      expr => compatibleTypes(fluent.typ, expr.typ),
      "has-compatible-type")

    /** Reads an identifier or construct a default one otherwise */
    val assertionId: Parser[String] =
      (freeIdent ~ ":" ~/ Pass) | PassWith(defaultId())

    assertionId.sideEffect(id = _).silent ~
      (timedSymExpr.sideEffect(fluent = _).silent ~
        (("==" ~/ rightSideExpr ~ (":->" ~/ rightSideExpr).?).map {
          case (expr, None)     => TimedEqualAssertion(fluent, expr, Some(ctx), id)
          case (from, Some(to)) => TimedTransitionAssertion(fluent, from, to, Some(ctx), id)
        } | (":=" ~/ rightSideExpr).map(e => TimedAssignmentAssertion(fluent, e, Some(ctx), id))))
  }

  val temporallyQualifiedAssertion: Parser[Seq[TemporallyQualifiedAssertion]] = {
    (interval ~/
      ((("{" ~ (!"}" ~/ timedAssertion ~ ";").rep ~ "}") |
        timedAssertion.map(Seq(_)))
        ~ ";"))
      .map { case (it, assertions) => assertions.map(TemporallyQualifiedAssertion(it, _)) }
  }

  val staticAssertion: Parser[StaticAssertion] =
    (staticSymExpr ~/
      (("==" | "!=").! ~/ staticSymExpr).? ~
      ";")
      .namedFilter({
        case (_, Some(_)) => true
        case (expr, None) => expr.typ.id.name == "boolean"
      }, "boolean-if-no-right-side")
      .namedFilter(
        {
          case (left, Some((_, right))) =>
            left.typ.isSubtypeOf(right.typ) || right.typ.isSubtypeOf(left.typ)
          case (_, None) => true // already checked that it is a boolean
        },
        "equality-between-compatible-types"
      )
      .map {
        case (left, Some(("==", right))) => StaticEqualAssertion(left, right)
        case (left, Some(("!=", right))) => StaticDifferentAssertion(left, right)
        case (expr, None)                => StaticEqualAssertion(expr, ctx.findVariable("true").get)
      }
}

/** Second phase parser that extracts all ANML elements expects types that should
  *  be already present in the initial model. */
class AnmlModuleParser(val initialModel: Model) extends AnmlParser(initialModel) {

  /** Parser for instance declaration.
    * "instance Type id1, id2, id3;" */
  val instancesDeclaration: Parser[Seq[InstanceDeclaration]] = {

    /** Parses a sequences of yet unused *distinct* identifiers. */
    def distinctFreeIdents(previous: Seq[String], sep: String, term: String): Parser[Seq[String]] =
      Pass ~ freeIdent
        .namedFilter(!previous.contains(_), "not-in-same-instance-list")
        .flatMap(name =>
          Pass ~ sep ~/ distinctFreeIdents(previous :+ name, sep, term)
            | Pass ~ term ~ PassWith(previous :+ name))

    (instanceKW ~/ declaredType ~/ distinctFreeIdents(Nil, ",", ";"))
      .map { case (typ, instanceNames) => instanceNames.map(name => Instance(ctx.id(name), typ)) }
  }.map(instances => instances.map(InstanceDeclaration(_)))

  /** Parser that to read the kind and type of a function declaration. For instance:
    * "fluent T", "constant T", "function T", "variable T", "predicate" where T is a type already declared.
    * Returns either ("fluent", T) or ("constant", T) considering that
    * 1) "variable" and "function" are alias for "fluent"
    * 2) "predicate" is an alias for "fluent boolean" */
  private[this] val functionKindAndType: Parser[(String, Type)] = {
    (word
      .filter(w => w == "fluent" || w == "variable" || w == "function")
      .opaque("fluent")
      .silent ~/ declaredType).map(("fluent", _)) |
      (constantKW ~/ declaredType).map(("constant", _)) |
      word
        .filter(_ == "predicate")
        .opaque("predicate")
        .optGet(_ => ctx.findType("boolean"), "with-boolean-type-in-scope")
        .map(("fluent", _))
  }

  val functionDeclaration: Parser[FunctionDeclaration] = {
    (functionKindAndType ~ freeIdent ~ argList ~ ";")
      .map {
        case ("fluent", typ, svName, args) =>
          FluentTemplate(ctx.id(svName), typ, args.map {
            case (name, argType) => Arg(Id(ctx.scope + svName, name), argType)
          })
        case ("constant", typ, svName, args) =>
          ConstantTemplate(ctx.id(svName), typ, args.map {
            case (name, argType) => Arg(Id(ctx.scope + svName, name), argType)
          })
        case _ => sys.error("Match failed")
      }
      .map(FunctionDeclaration(_))
  }

  /** Extract the functions declared in a type. This consumes the whole type declaration.
    * Note that the type should be already present in the module.
    * Typically consumed:
    *   "type B < A with { fluent f(C c); };" */
  val inTypeFunctionDeclaration: Parser[Seq[FunctionDeclaration]] =
    (typeKW ~/
      declaredType
      ~ ("<" ~/ declaredType.!).asInstanceOf[Parser[Type]].?
      ~ (withKW ~/ "{" ~/ functionDeclaration.rep ~ "}").?
      ~ ";")
      .map {
        case (t, _, None) => Seq()
        case (t, _, Some(funcDecl)) =>
          funcDecl.map(fd => {
            val id            = Id(t.asScope, fd.id.name)
            val functionScope = t.asScope + id.name
            val selfArg       = Arg(Id(functionScope, "self"), t)
            val params = selfArg +: fd.func.params.map(arg =>
              Arg(Id(functionScope, arg.id.name), arg.typ))
            val template = fd.func match {
              case _: FluentTemplate   => FluentTemplate(id, fd.func.typ, params)
              case _: ConstantTemplate => ConstantTemplate(id, fd.func.typ, params)
            }
            FunctionDeclaration(template)
          })
      }

  val action: Parser[ActionTemplate] = new AnmlActionParser(this).parser

  val elem: Parser[Seq[ModuleElem]] =
    inTypeFunctionDeclaration |
      instancesDeclaration |
      functionDeclaration.map(Seq(_)) |
      timepointDeclaration.map(Seq(_)) |
      temporalConstraint |
      temporallyQualifiedAssertion |
      staticAssertion.map(Seq(_)) |
      action.map(Seq(_))

  def currentModel: Model = ctx match {
    case m: Model => m
    case _        => sys.error("Current context is not a model")
  }

  private[this] def anmlParser: Parser[Model] =
    Pass ~/ (End ~ PassWith(currentModel) |
      (elem ~ Pass).flatMap(elem =>
        currentModel ++ elem match {
          case Some(extended) =>
            updateContext(extended) // change state
            anmlParser
          case None => Fail.opaque("fail: parsed elem does not fit into the previous model")
      }))

  def parse(input: String) = {
    updateContext(initialModel)
    anmlParser.parse(input)
  }
}

class AnmlActionParser(superParser: AnmlModuleParser) extends AnmlParser(superParser.currentModel) {

  private def currentAction: ActionTemplate = ctx match {
    case a: ActionTemplate => a
    case _                 => sys.error("Current context is not an action.")
  }

  /** Creates an action template with the given name and its default content (i.e. start/end timepoints). */
  private[this] def buildEmptyAction(actionName: String): ActionTemplate = {
    val container = ctx match {
      case m: Model => m
      case _        => sys.error("Starting to parse an action while the context is not a model.")
    }
    val emptyAct = new ActionTemplate(actionName, container, Nil)
    emptyAct +
      TimepointDeclaration(TPRef(Id(emptyAct.scope, "start"))) +
      TimepointDeclaration(TPRef(Id(emptyAct.scope, "end")))
  }

  val parser: Parser[ActionTemplate] =
    (Pass ~ actionKW.sideEffect(x => {
      // set context to the current model in order to access previous declarations
      updateContext(superParser.currentModel)
    }) ~/
      freeIdent.sideEffect(actionName => {
        // set context to the current action
        updateContext(buildEmptyAction(actionName))
      }) ~/
      argList // parse arguments and update the current action
        .map(_.map {
          case (name, typ) => ArgDeclaration(Arg(Id(ctx.scope, name), typ))
        })
        .sideEffect(argDeclarations => updateContext(currentAction ++ argDeclarations)) ~
      "{" ~/
      (temporalConstraint |
        temporallyQualifiedAssertion |
        staticAssertion.map(Seq(_)))
        .sideEffect(x => updateContext(currentAction ++ x)) // add assertions to the current action
        .rep ~
      "}" ~/
      ";")
      .flatMap(_ => PassWith(currentAction))
}

/** First phase parser used to extract all type declarations from a given ANML string. */
class AnmlTypeParser(val initialModel: Model) extends AnmlParser(initialModel) {

  val nonTypeToken =
    (word | int | CharIn("{}[]();=:<>-+.,!/*")).!.namedFilter(_ != "type", "non-type-token")
  val typeDeclaration = (typeKW ~/ freeIdent ~ ("<" ~ declaredType).? ~ (";" | withKW)).map {
    case (name, parentOpt) => TypeDeclaration(Type(ctx.id(name), parentOpt))
  }

  private[this] def currentModel: Model = ctx match {
    case m: Model => m
    case x        => sys.error("Current context is not a model")
  }

  private[this] def parser: Parser[Model] =
    Pass ~/ (End ~ PassWith(currentModel) |
      (nonTypeToken ~/ Pass).flatMap(_ => parser) |
      (typeDeclaration ~ Pass).flatMap(typeDecl =>
        currentModel + typeDecl match {
          case Some(extendedModel) =>
            updateContext(extendedModel)
            parser
          case None => Fail
      }))

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
