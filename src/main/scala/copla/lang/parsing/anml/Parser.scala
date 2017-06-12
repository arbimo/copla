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
  val typeKW: Parser[Unit] = word.filter(_ == "type").map(x => {}).opaque("type")
  val instanceKW           = word.filter(_ == "instance").map(_ => {}).opaque("instance")
  val fluentKW             = word.filter(_ == "fluent").map(_ => {}).opaque("instance")
  val timepointKW          = word.filter(_ == "timepoint").map(_ => {}).opaque("instance")
  val keywords             = Set("type", "instance", "action", "duration", "fluent", "predicate", "timepoint")
  val reservedTypeNames    = Set()
  val nonIdent             = keywords ++ reservedTypeNames

  val ident                    = word.filter(!nonIdent.contains(_)).opaque("identifier")
  val typeName: Parser[String] = word.filter(!keywords.contains(_)).opaque("type-name")
  val variableName             = ident.opaque("variable-name")

  def isFree(id: String, c: Ctx) =
    c.findType(id).orElse(c.findTimepoint(id)).orElse(c.findVariable(id)).isEmpty &&
      !keywords.contains(id)

  def declaredType(c: Model): Parser[Type] =
    typeName
      .flatMap(c.findType(_) match {
        case Some(t) => PassWith(t)
        case None    => Fail
      })
      .opaque("previously-declared-type")

  def typeDeclaration(c: Model): Parser[Type] =
    (typeKW ~/
      typeName
        .filter(isFree(_, c))
        .opaque("previously-undefined-type")
        .!
      ~ ("<" ~/ declaredType(c).!).asInstanceOf[Parser[Type]].?
      ~ ";")
      .map { case (name, parentOpt) => Type(c.id(name), parentOpt) }
      .opaque("type-declaration")

  /** Parser for instance declaration.
    * "instance Type id1, id2, id3;" */
  def instancesDeclaration(m: Model): Parser[Seq[Instance]] = {

    /** Parses a sequences of yet unused *distinct* identifiers. */
    def distinctFreeIdents(m: Model,
                           previous: Seq[String],
                           sep: String,
                           term: String): Parser[Seq[String]] =
      Pass ~ ident
        .filter(isFree(_, m))
        .filter(!previous.contains(_))
        .opaque("free-identifier")
        .flatMap(name =>
          Pass ~ sep ~/ distinctFreeIdents(m, previous :+ name, sep, term)
            | Pass ~ term ~ PassWith(previous :+ name))

    (instanceKW ~/ declaredType(m) ~/ distinctFreeIdents(m, Nil, ",", ";"))
      .map { case (typ, instanceNames) => instanceNames.map(name => Instance(m.id(name), typ)) }
  }.opaque("instance declaration")

  protected def arg(m: Model): Parser[(String, Type)] =
    (declaredType(m) ~ ident.filter(!keywords.contains(_)))
      .opaque("argument-declaration")
      .map { case (typ, argName) => (argName, typ) }

  /** A liest of at least one argument formated as "Type1 arg, Type2 arg2" */
  protected def distinctArgSeq(
      m: Model,
      sep: String,
      previous: Seq[(String, Type)] = Seq()): Parser[Seq[(String, Type)]] =
    Pass ~ arg(m)
      .filter(a => !previous.exists(_._1 == a._1)) // disallow args with same name
      .opaque("argument-declaration")
      .flatMap(a =>
        (Pass ~ sep ~/ distinctArgSeq(m, sep, previous :+ a)) | PassWith(previous :+ a))

  /** Parses a sequence of args necessarily enclosed in parenthesis if non empty
    * Example of valid inputs "", "()", "(Type1 arg1)", "(Type1 arg1, Type2 arg2)"
    */
  protected def args(m: Model): Parser[Seq[(String, Type)]] =
    ("(" ~ (distinctArgSeq(m, ",") | PassWith(Seq())) ~ ")") | // parenthesis with and without args
      PassWith(Seq()) // no args no, parenthesis

  def stateVariableDeclaration(m: Model): Parser[StateVariableTemplate] =
    (fluentKW ~/ declaredType(m) ~ ident.filter(isFree(_, m)) ~ args(m) ~ ";")
      .map {
        case (typ, svName, args) =>
          StateVariableTemplate(svName, typ, args.map {
            case (name, argType) => Arg(Id(m.scope + svName, name), argType)
          })
      }

  def timepointDeclation(c: Ctx): Parser[TPRef] =
    timepointKW ~/ ident.filter(isFree(_, c)).map(name => TPRef(c.id(name))) ~ ";"

  def elem(m: Model): Parser[Seq[ModuleElem]] =
    typeDeclaration(m).map(Seq(_)) |
      instancesDeclaration(m) |
      stateVariableDeclaration(m).map(Seq(_)) |
      timepointDeclation(m).map(Seq(_))

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
