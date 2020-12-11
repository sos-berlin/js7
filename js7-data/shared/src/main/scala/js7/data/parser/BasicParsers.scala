package js7.data.parser

import fastparse.NoWhitespace._
import fastparse._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits._
import js7.base.utils.Identifier.{isIdentifierPart, isIdentifierStart}
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentName
import js7.data.folder.FolderPath
import js7.data.item.ItemPath
import js7.data.lock.LockName
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object BasicParsers
{
  private def inlineCommentUntilStar[_: P] = P(CharsWhile(_ != '*', 0) ~ P("*"))

  private def inlineComment[_: P] = P {
    P("/*") ~ inlineCommentUntilStar ~ (!"/" ~ inlineCommentUntilStar).rep ~ P("/")
  }
  private def lineEndComment[_: P] = P("//" ~ CharsWhile(_ != '\n'))
  private def comment[_: P] = P(inlineComment | lineEndComment)
  /** Optional whitespace including line ends */
  def w[_: P] = P((CharsWhileIn(" \t\r\n") | comment).rep)
  /** Optional horizontal whitespace */
  def h[_: P] = P((CharsWhileIn(" \t") | comment).rep)
  def comma[_: P] = P(w ~ "," ~ w)
  //def newline[_: P] = P(h ~ "\r".? ~ "\n" ~ w)
  //def commaOrNewLine[_: P] = P(h ~ ("," | (newline ~ w ~ ",".?)) ~ w)
  def int[_: P] = P[Int](("-".? ~ digits).!.map(_.toInt))
  private def digits[_: P] = P(CharsWhile(c => c >= '0' && c <= '9'))
  def identifierEnd[_: P] = P(&(CharPred(c => !isIdentifierPart(c))) | End)
  def identifier[_: P] = P[String](  // TODO Compare and test code with Identifier.isIdentifier
    (CharPred(isIdentifierStart).opaque("identifier start") ~ CharsWhile(isIdentifierPart, 0)).! ~
      identifierEnd)

  def quotedString[_: P] = P[String](
    doubleQuoted | singleQuoted)

  def singleQuoted[_: P] = P[String](
    //singleQuotedTooLong(6) |
      singleQuotedN(5) |
      singleQuotedN(4) |
      singleQuotedN(3) |
      singleQuotedN(2) |
      singleQuoted1)

  private def singleQuoted1[_: P] = P[String](
    ("'" ~~/
      singleQuotedContent ~~
      "'".opaque("properly terminated '…'-quoted string without non-printable characters (except \\r or \\n)"))
    .map(_.replace("\r\n", "\n")))

  private def singleQuotedN[_: P](n: Int) = P[String](
    (("'" * n) ~~/
      singleQuotedContent ~~
      ("'".rep(min = 1, max = n - 1).! ~~ !"'" ~~ singleQuotedContent).rep ~~
      ("'" * n).opaque(s"properly terminated ${"'" * n}…${"'" * n}-quoted string without non-printable characters (except \\t, \\r and \\n) — or use ${"\"\""} (not '') for the empty string"))
    .map { case (head, pairs) =>
      (head + pairs.map { case (a, b) => a + b }.mkString).replace("\r\n", "\n") })

  private def singleQuotedContent[_: P] = P[String](
    CharsWhile(ch => ch != '\'' && (ch >= ' ' || ch == '\r' || ch == '\n'), /*min=*/0).!)

  //private def singleQuotedTooLong[_: P](n: Int) = P[String](
  //  P("'" * n).flatMap(_ => invalid(s"More than ${n - 1} '-quotes are not supported")))

  private def doubleQuoted[_: P] = P[String](
    "\"" ~~/
      doubleQuotedContent ~~
      "\"".opaque("""properly terminated "…"-quoted string"""))

  private def doubleQuotedContent[_: P] = P[String](
    (doubleQuotedContentPart ~~/ ("\\" ~~/ escapedChar ~~ doubleQuotedContentPart).rep(0))
      .map { case (a, pairs) => a + pairs.map(o => o._1 + o._2).mkString })

  private def doubleQuotedContentPart[_: P] = P[String](
    CharsWhile(ch => ch != '"' && ch != '\\' && ch != '$'/*reserved for interpolation*/ /*&& ch >= ' '*/, 0).!)

  private def escapedChar[_: P] = P[String](
    SingleChar.!.flatMap {
      case "\\" => valid("\\")
      case "\"" => valid("\"")
      case "r" => valid("\r")
      case "n" => valid("\n")
      case "t" => valid("\t")
      case "$" => valid("$")
      case _ => invalid("""blackslash (\) and one of the following characters: [\"nt]""")
    })

  def pathString[_: P] = P[String](quotedString)

  def keyword[_: P] = P(identifier)

  def keyword[_: P](name: String) = P[Unit](name ~ identifierEnd)

  def path[A <: ItemPath: ItemPath.Companion](implicit ctx: P[_]) = P[A](
    pathString.map(p => FolderPath.Root.resolve[A](p)))

  def agentName(implicit ctx: P[_]) = P[AgentName](
    pathString.flatMap(string =>
      AgentName.checked(string) match {
        case Left(problem) => Fail.opaque(problem.toString)
        case Right(name) => Pass(name)
      }))

  def lockName(implicit ctx: P[_]) = P[LockName](
    pathString.flatMap(string =>
      LockName.checked(string) match {
        case Left(problem) => Fail.opaque(problem.toString)
        case Right(name) => Pass(name)
      }))

  def keyValues[A](namedValueParser: => P[(String, A)])(implicit ctx: P[_]) = P[KeyToValue[A]](
    commaSequence(namedValueParser).flatMap(namedValues =>
      namedValues.duplicateKeys(_._1) match {
        case Some(dups) => Fail.opaque("unique keywords (duplicates: " + dups.keys.mkString(", ") + ")")
        case None => Pass(KeyToValue(namedValues.toMap))
      }))

  def keyValue[A](name: String, parser: => P[A])(implicit ctx: P[_]): P[(String, A)] =
    keyValueConvert(name, parser)(Checked.apply[A])

  def keyValueConvert[A, B](name: String, parser: => P[A])(toValue: A => Checked[B])(implicit ctx: P[_]) = P[(String, B)](
    w ~ specificKeyValue(name, parser).flatMap(o => checkedToP(toValue(o))).map(name.->))

  final case class KeyToValue[A](nameToValue: Map[String, A]) {
    def apply[A1 <: A](key: String, default: => A1)(implicit ctx: P[_]): P[A1] =
      Pass(nameToValue.get(key).fold(default)(_.asInstanceOf[A1]))

    def apply[A1 <: A: ClassTag](key: String)(implicit ctx: P[_]): P[A1] =
      nameToValue.get(key) match {
        case None => Fail.opaque(s"keyword $key=")
        case Some(o) =>
          if (!implicitClass[A1].isAssignableFrom(o.getClass))
            Fail.opaque(s"keyword $key=<${implicitClass[A1].simpleScalaName}>, not $key=<${o.getClass.simpleScalaName}>")
          else
            Pass(o.asInstanceOf[A1])
      }

    def get[A1 <: A](key: String)(implicit ctx: P[_]): P[Option[A1]] =
      Pass(nameToValue.get(key).map(_.asInstanceOf[A1]))

    def noneOrOneOf[A1 <: A](keys: String*)(implicit ctx: P[_]): P[Option[(String, A1)]] =
      noneOrOneOf[A1](keys.toSet)

    def noneOrOneOf[A1 <: A](keys: Set[String])(implicit ctx: P[_]): P[Option[(String, A1)]] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        case 0 => Pass(None)
        case 1 => Pass(Some(intersection.head -> nameToValue(intersection.head).asInstanceOf[A1]))
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOf[A1 <: A](keys: String*)(implicit ctx: P[_]): P[(String, A1)] =
      oneOf[A1](keys.toSet)

    def oneOf[A1 <: A](keys: Set[String])(implicit ctx: P[_]): P[(String, A1)] = {
      val intersection = nameToValue.keySet & keys
      intersection.size match {
        // TODO Better messages for empty key (no keyword, positional argument)
        case 0 => Fail.opaque("keywords " + keys.map(_ + "=").mkString(", "))
        case 1 => Pass(intersection.head -> nameToValue(intersection.head).asInstanceOf[A1])
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOfOr[A1 <: A](keys: Set[String], default: A1)(implicit ctx: P[_]): P[A1] = {
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

  def specificKeyValue[V](name: String, valueParser: => P[V])(implicit ctx: P[_]) = P[V] {
    def keywordPart = if (name.isEmpty) Pass else keyword(name) ~ w ~ "=" ~/ w
    P(keywordPart ~ valueParser)
  }

  def curly[A](parser: => P[A])(implicit ctx: P[_]) = P[A](
    h ~ "{" ~ w ~/ parser ~ w ~ "}")

  def inParentheses[A](parser: => P[A])(implicit ctx: P[_]) = P[A](
    h ~ "(" ~ w ~/ parser ~ w ~ ")")

  def bracketCommaSequence[A](parser: => P[A])(implicit ctx: P[_]) = P[collection.Seq[A]](
    "[" ~ w ~/ commaSequence(parser) ~ w ~ "]")

  def commaSequence[A](parser: => P[A])(implicit ctx: P[_]) = P[collection.Seq[A]](
    nonEmptyCommaSequence(parser).?.map(_ getOrElse Nil))

  def nonEmptyCommaSequence[A](parser: => P[A])(implicit ctx: P[_]) = P[collection.Seq[A]](
    parser ~ (comma ~/ parser).rep map {
      case (head, tail) => head +: tail
    })

  def leftRecurse[A, O](operand: => P[A], operator: => P[O])(operation: (A, O, A) => P[A])(implicit ctx: P[_]): P[A] = {
    // Separate function variable to work around a crash (fastparse 2.1.0)
    def more(implicit ctx: P[_]) = (w ~ operator ~ w ~/ operand).rep
    (operand ~ more)
      .flatMap {
        case (head, tail) =>
          def loop(left: A, tail: List[(O, A)]): P[A] =
            tail match {
              case Nil =>
                valid(left)
              case (op, right) :: tl =>
                operation(left, op, right).flatMap(a => loop(a, tl))
            }
          loop(head, tail.toList)
      }
  }

  def valid[A](a: A)(implicit ctx: P[_]): P[A] = checkedToP(Right(a))

  def invalid[_: P](message: String): P[Nothing] = checkedToP(Problem.pure(message))

  def checkedToP[A](checked: Checked[A])(implicit ctx: P[_]): P[A] =
    checked match {
      case Right(a) => Pass(a)
      case Left(problem) => Fail.opaque(problem.toString)
    }
}
