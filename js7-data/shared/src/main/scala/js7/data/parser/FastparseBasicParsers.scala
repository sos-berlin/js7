package js7.data.parser

import fastparse.*
import fastparse.NoWhitespace.*
import java.lang.Character.isUnicodeIdentifierPart
import js7.base.parser.BasicPrinter.{isIdentifierPart, isIdentifierStart}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.item.VersionedItemPath
import js7.data.lock.LockPath
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object FastparseBasicParsers
{
  private def inlineCommentUntilStar[x: P] = P(CharsWhile(_ != '*', 0) ~ P("*"))

  private def inlineComment[x: P] = P {
    P("/*") ~ inlineCommentUntilStar ~ (!"/" ~ inlineCommentUntilStar).rep ~ P("/")
  }
  private def lineEndComment[x: P] = P("//" ~ CharsWhile(_ != '\n'))
  private def comment[x: P] = P(inlineComment | lineEndComment)
  /** Optional whitespace including line ends */
  def w[x: P] = P((CharsWhileIn(" \t\r\n") | comment).rep)
  /** Optional horizontal whitespace */
  def h[x: P] = P((CharsWhileIn(" \t") | comment).rep)
  def comma[x: P] = P(w ~ "," ~ w)
  //def newline[x: P] = P(h ~ "\r".? ~ "\n" ~ w)
  //def commaOrNewLine[x: P] = P(h ~ ("," | (newline ~ w ~ ",".?)) ~ w)
  def int[x: P] = P[Int](("-".? ~ digits).!.map(_.toInt))

  def bigDecimal[x: P] = P[BigDecimal](
    ("-".? ~ digits ~ ("." ~ digits).?).!
      .flatMap(o => checkedToP(Checked.catchNonFatal(BigDecimal(o)))))

  def digits[x: P] = P[String](CharsWhile(c => c >= '0' && c <= '9').!)

  def identifier[x: P] = P[String](
    simpleIdentifier | backtickIdentifier)

  def simpleIdentifier[x: P] = P[String](
    (CharPred(isIdentifierStart).opaque("identifier start") ~ CharsWhile(isIdentifierPart, 0)).!)

  //def js7Path[x: P] = P[String](
  //  (js7Name ~ ("/" ~ js7Name).rep).!)
  //
  //def js7Name[x: P] = P(String) {
  //  import Js7PathValidator.Standard.js7NameValidator
  //  (CharPred(js7NameValidator.isNameStart).opaque("name start") ~
  //    CharsWhile(isIdentifierPart, 0)).!
  //}

  private def backtickIdentifier[x: P] = P[String](
    rawIdentifierPart.rep(1).map(_.mkString("`"))
      .flatMap {
        case "" => Fail.opaque("Identifier in backticks ` must not be empty")
        case o => Pass(o)
      })

  private def rawIdentifierPart[x: P] = P[String](
    "`" ~/ CharsWhile(c => c != '`' && c != '\n', 0).! ~/ "`")

  def identifierEnd[x: P] = P(&(CharPred(c => !isUnicodeIdentifierPart(c))) | End)

  def keyword[x: P] = P(
    (CharPred(isIdentifierStart).opaque("keyword start") ~
      CharsWhile(isUnicodeIdentifierPart, 0)
    ).!)

  def keyword[x: P](name: String) = P[Unit](name ~ identifierEnd)

  def quotedString[x: P] = P[String](
    doubleQuoted | singleQuoted)

  def singleQuoted[x: P] = P[String](
    //singleQuotedTooLong(6) |
      singleQuotedN(5) |
      singleQuotedN(4) |
      singleQuotedN(3) |
      singleQuotedN(2) |
      singleQuoted1)

  private def singleQuoted1[x: P] = P[String](
    ("'" ~~/
      singleQuotedContent ~~
      "'".opaque("properly terminated '…'-quoted string without non-printable characters (except \\r or \\n)"))
    .map(_.replace("\r\n", "\n")))

  private def singleQuotedN[x: P](n: Int) = P[String](
    (("'" * n) ~~/
      singleQuotedContent ~~
      ("'".rep(min = 1, max = n - 1).! ~~ !"'" ~~ singleQuotedContent).rep ~~
      ("'" * n).opaque(s"properly terminated ${"'" * n}…${"'" * n}-quoted string without non-printable characters (except \\t, \\r and \\n) — or use ${"\"\""} (not '') for the empty string"))
    .map { case (head, pairs) =>
      (head + pairs.map { case (a, b) => a + b }.mkString).replace("\r\n", "\n") })

  private def singleQuotedContent[x: P] = P[String](
    CharsWhile(ch => ch != '\'', /*min=*/0).!)

  //private def singleQuotedTooLong[x: P](n: Int) = P[String](
  //  P("'" * n).flatMap(_ => invalid(s"More than ${n - 1} '-quotes are not supported")))

  private def doubleQuoted[x: P] = P[String](
    "\"" ~~/
      doubleQuotedContent ~~
      "\"".opaque("""properly terminated "…"-quoted string"""))

  private def doubleQuotedContent[x: P] = P[String](
    (doubleQuotedContentPart ~~/ (escapedCharInString ~~ doubleQuotedContentPart).rep(0))
      .map { case (a, pairs) => a + pairs.map(o => o._1 + o._2).mkString })

  private def doubleQuotedContentPart[x: P] = P[String](
    CharsWhile(ch => ch != '"' && ch != '\\' && ch != '$'/*reserved for interpolation*/ /*&& ch >= ' '*/, 0).!)

  def escapedCharInString[x: P] = P[String](
    "\\" ~~/
      SingleChar.flatMap {
        case '\\' => valid("\\")
        case '"' => valid("\"")
        case 't' => valid("\t")
        case 'r' => valid("\r")
        case 'n' => valid("\n")
        case '$' => valid("$")
        case _ => invalid("""blackslash (\) and one of the following characters: [\"trn$]""")
      })

  def pathString[x: P] = P[String](quotedString)

  def path[A <: VersionedItemPath](implicit ctx: P[?], A: VersionedItemPath.Companion[A]) = P[A](
    pathString.flatMap(p => checkedToP(A.checked(p))))

  def agentPath(implicit ctx: P[?]) = P[AgentPath](
    pathString.flatMap(string =>
      AgentPath.checked(string) match {
        case Left(problem) => Fail.opaque(problem.toString)
        case Right(name) => Pass(name)
      }))

  def quotedLockPath(implicit ctx: P[?]) = P[LockPath](
    quotedString.flatMap(string =>
      LockPath.checked(string) match {
        case Left(problem) => Fail.opaque(problem.toString)
        case Right(name) => Pass(name)
      }))

  def keyValues[A](namedValueParser: => P[(String, A)])(implicit ctx: P[?]) = P[KeyToValue[A]](
    commaSequence(namedValueParser).flatMap(namedValues =>
      namedValues.duplicateKeys(_._1) match {
        case Some(dups) => Fail.opaque("unique keywords (duplicates: " + dups.keys.mkString(", ") + ")")
        case None => Pass(KeyToValue(namedValues.toMap))
      }))

  def keyValue[A](name: String, parser: => P[A])(implicit ctx: P[?]): P[(String, A)] =
    keyValueConvert(name, parser)(Checked.apply[A])

  def keyValueConvert[A, B](name: String, parser: => P[A])(toValue: A => Checked[B])(implicit ctx: P[?]) = P[(String, B)](
    w ~ specificKeyValue(name, parser).flatMap(o => checkedToP(toValue(o))).map(name.->))

  final case class KeyToValue[A](nameToValue: Map[String, A]) {
    def apply[A1 <: A](key: String, default: => A1)(implicit ctx: P[?]): P[A1] =
      Pass(nameToValue.get(key).fold(default)(_.asInstanceOf[A1]))

    def apply[A1 <: A: ClassTag](key: String)(implicit ctx: P[?]): P[A1] =
      nameToValue.get(key) match {
        case None => Fail.opaque(s"keyword $key=")
        case Some(o) =>
          if (!implicitClass[A1].isAssignableFrom(o.getClass))
            Fail.opaque(s"keyword $key=<${implicitClass[A1].simpleScalaName}>, not $key=<${o.getClass.simpleScalaName}>")
          else
            Pass(o.asInstanceOf[A1])
      }

    def get[A1 <: A](key: String)(implicit ctx: P[?]): P[Option[A1]] =
      Pass(nameToValue.get(key).map(_.asInstanceOf[A1]))

    def noneOrOneOf[A1 <: A](keys: String*)(implicit ctx: P[?]): P[Option[(String, A1)]] =
      noneOrOneOf[A1](keys.toSet)

    def noneOrOneOf[A1 <: A](keys: Set[String])(implicit ctx: P[?]): P[Option[(String, A1)]] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        case 0 => Pass(None)
        case 1 => Pass(Some(intersection.head -> nameToValue(intersection.head).asInstanceOf[A1]))
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOf[A1 <: A](keys: String*)(implicit ctx: P[?]): P[(String, A1)] =
      oneOf[A1](keys.toSet)

    def oneOf[A1 <: A](keys: Set[String])(implicit ctx: P[?]): P[(String, A1)] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        // TODO Better messages for empty key (no keyword, positional argument)
        case 0 => Fail.opaque("keywords " + keys.map(_ + "=").mkString(", "))
        case 1 => Pass(intersection.head -> nameToValue(intersection.head).asInstanceOf[A1])
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOfOr[A1 <: A](keys: Set[String], default: A1)(implicit ctx: P[?]): P[A1] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        case 0 => Pass(default)
        case 1 => Pass(nameToValue(intersection.head).asInstanceOf[A1])
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }
  }
  object KeyToValue {
    val empty = KeyToValue[Any](Map.empty)
  }

  def specificKeyValue[V](name: String, valueParser: => P[V])(implicit ctx: P[?]) = P[V] {
    def keywordPart = if (name.isEmpty) Pass else keyword(name) ~ w ~ "=" ~/ w
    P(keywordPart ~ valueParser)
  }

  def curly[A](parser: => P[A])(implicit ctx: P[?]) = P[A](
    h ~ "{" ~ w ~/ parser ~ w ~ "}")

  def inParentheses[A](parser: => P[A])(implicit ctx: P[?]) = P[A](
    h ~ "(" ~ w ~/ parser ~ w ~ ")")

  def bracketCommaSequence[A](parser: => P[A])(implicit ctx: P[?]) = P[Seq[A]](
    "[" ~ w ~/ commaSequence(parser) ~ w ~ "]")

  def commaSequence[A](parser: => P[A])(implicit ctx: P[?]) = P[Seq[A]](
    nonEmptyCommaSequence(parser).?.map(_ getOrElse Nil))

  def nonEmptyCommaSequence[A](parser: => P[A])(implicit ctx: P[?]) = P[Seq[A]](
    parser ~ (comma ~/ parser).rep map {
      case (head, tail) => head +: tail
    })

  def leftRecurse[A, O](initial: A, operator: => P[O], operand: => P[A])(operation: (A, O, A) => P[A])(implicit ctx: P[?]): P[A] = {
    // Separate function variable to work around a crash (fastparse 2.1.0)
    def more(implicit ctx: P[?]) = (w ~ operator ~ w ~/ operand).rep
    more.flatMap { tail =>
      def loop(left: A, tail: List[(O, A)]): P[A] =
        tail match {
          case Nil =>
            valid(left)
          case (op, right) :: tl =>
            operation(left, op, right).flatMap(a => loop(a, tl))
        }
      loop(initial, tail.toList)
    }
  }

  def valid[A](a: A)(implicit ctx: P[?]): P[A] = checkedToP(Right(a))

  def invalid[x: P](message: String): P[Nothing] = checkedToP(Problem.pure(message))

  def checkedToP[A](checked: Checked[A])(implicit ctx: P[?]): P[A] =
    checked match {
      case Right(a) => Pass(a)
      case Left(problem) => Fail.opaque(problem.toString)
    }
}
