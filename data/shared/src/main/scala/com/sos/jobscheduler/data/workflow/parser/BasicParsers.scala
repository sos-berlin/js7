package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.Identifier.{isIdentifierPart, isIdentifierStart}
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import fastparse.NoWhitespace._
import fastparse._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
private[parser] object BasicParsers
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
  def int[_: P] = P[Int](("-".? ~ digits).! map (_.toInt))
  private def digits[_: P] = P(CharsWhile(c => c >= '0' && c <= '9'))
  def identifierEnd[_: P] = P(&(CharPred(c => !isIdentifierPart(c))) | End)
  def identifier[_: P] = P[String](
    (CharPred(isIdentifierStart).opaque("identifier start") ~ CharsWhile(isIdentifierPart, 0)).! ~
      identifierEnd)

  def quotedString[_: P] = P[String](
    singleQuoted | doubleQuoted)

  private def singleQuoted[_: P] = P[String](
    "'" ~/
      CharsWhile(ch => ch != '\'' && ch >= ' ', 0).! ~
      "'".opaque("properly terminated single-quoted (') string without non-printable characters"))

  private def doubleQuoted[_: P] = P[String](
    ("\"" ~/
      CharsWhile(ch => ch != '"' && ch >= ' ' && ch != '\\', 0).! ~
      "\"".opaque("""properly terminated double-quoted (") string without non-printable character nor backslash (\)""")
    ).flatMap {
      case o if o contains '$' =>
        invalid("double-quoted string without variable interpolation via '$' (consider using single quotes ('))")
      case o => valid(o)
    })

  def pathString[_: P] = P[String](quotedString)

  def keyword[_: P] = P(identifier)

  def keyword[_: P](name: String) = P[Unit](name ~ identifierEnd)

  def path[A <: TypedPath: TypedPath.Companion](implicit ctx: P[_]) = P[A](
    pathString map (p => FolderPath.Root.resolve[A](p)))

  def keyValues[A](keyValueParser: => P[(String, A)])(implicit ctx: P[_]) = P[KeyToValue[A]](
    commaSequence(keyValueParser).flatMap(keyValues =>
      keyValues.duplicateKeys(_._1) match {
        case Some(dups) => Fail.opaque("unique keywords (duplicates: " + dups.keys.mkString(", ") + ")")
        case None => Pass(KeyToValue(keyValues.toMap))
      }))

  def keyValue[A](name: String, parser: => P[A])(implicit ctx: P[_]): P[(String, A)] =
    keyValue(name, parser, identity[A])

  def keyValue[A, B](name: String, parser: => P[A], toValue: A => B)(implicit ctx: P[_]): P[(String, B)] =
    specificKeyValue(name, parser).map(o => name -> toValue(o))

  final case class KeyToValue[A](keyToValue: Map[String, A]) {
    def apply[A1 <: A](key: String, default: => A1)(implicit ctx: P[_]): P[A1] =
      Pass(keyToValue.get(key).fold(default)(_.asInstanceOf[A1]))

    def apply[A1 <: A: ClassTag](key: String)(implicit ctx: P[_]): P[A1] =
      keyToValue.get(key) match {
        case None => Fail.opaque(s"keyword $key=")
        case Some(o) =>
          if (!implicitClass[A1].isAssignableFrom(o.getClass))
            Fail.opaque(s"keyword $key=<${implicitClass[A1].simpleScalaName}>, not $key=<${o.getClass.simpleScalaName}>")
          else
            Pass(o.asInstanceOf[A1])
      }

    def get[A1 <: A](key: String)(implicit ctx: P[_]): P[Option[A1]] =
      Pass(keyToValue.get(key) map (_.asInstanceOf[A1]))

    def noneOrOneOf[A1 <: A](keys: Set[String])(implicit ctx: P[_]): P[Option[(String, A1)]] = {
      val intersection = keyToValue.keySet & keys
      intersection.size match {
        case 0 => Pass(None)
        case 1 => Pass(Some(intersection.head -> keyToValue(intersection.head).asInstanceOf[A1]))
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOf[A1 <: A](keys: Set[String])(implicit ctx: P[_]): P[(String, A1)] = {
      val intersection = keyToValue.keySet & keys
      intersection.size match {
        case 0 => Fail.opaque("keywords " + keys.map(_ + "=").mkString(", "))
        case 1 => Pass(intersection.head -> keyToValue(intersection.head).asInstanceOf[A1])
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOfOr[A1 <: A](keys: Set[String], default: A1)(implicit ctx: P[_]): P[A1] = {
      val intersection = keyToValue.keySet & keys
      intersection.size match {
        case 0 => Pass(default)
        case 1 => Pass(keyToValue(intersection.head).asInstanceOf[A1])
        case _ => Fail.opaque(s"non-contradicting keywords: ${intersection.mkString("; ")}")
      }
    }
  }

  def specificKeyValue[V](name: String, valueParser: => P[V])(implicit ctx: P[_]): P[V] =
    P(keyword(name) ~ w ~ "=" ~ w ~/ valueParser)

  def curly[A](parser: => P[A])(implicit ctx: P[_]) = P[A](
    h ~ "{" ~ w ~/ parser ~ w ~ "}")

  def inParentheses[A](parser: => P[A])(implicit ctx: P[_]) = P[A](
    h ~ "(" ~ w ~/ parser ~ w ~ ")")

  def bracketCommaSequence[A](parser: => P[A])(implicit ctx: P[_]) = P[collection.Seq[A]](
    "[" ~ w ~/ commaSequence(parser) ~ w ~ "]")

  def commaSequence[A](parser: => P[A])(implicit ctx: P[_]) = P[collection.Seq[A]](
    nonEmptyCommaSequence(parser).? map (_ getOrElse Nil))

  def nonEmptyCommaSequence[A](parser: => P[A])(implicit ctx: P[_]) = P[collection.Seq[A]](
    parser ~ (comma ~/ parser).rep ~ w map {
      case (head, tail) => head +: tail
    })

  def leftRecurse[A, O](operand: => P[A], operator: => P[O])(operation: (A, O, A) => P[A])(implicit ctx: P[_]): P[A] = {
    // Separate function variable to work around a crash (fastparse 2.1.0)
    def more(implicit ctx: P[_]) = (w ~ operator ~ w ~/ operand).rep(0)
    (operand ~ more)
      .flatMap {
        case (head, tail) =>
          def loop(left: A, tail: List[(O, A)]): P[A] =
            tail match {
              case Nil =>
                valid(left)
              case (op, right) :: tl =>
                operation(left, op, right) flatMap (a => loop(a, tl))
            }
          loop(head, tail.toList)
      }
  }

  def valid[A](a: A)(implicit ctx: P[_]): P[A] = checkedToP(Valid(a))

  def invalid[_: P](message: String): P[Nothing] = checkedToP(Problem.pure(message))

  def checkedToP[A](checked: Checked[A])(implicit ctx: P[_]): P[A] =
    checked match {
      case Valid(a) => Pass(a)
      case Invalid(problem) => Fail.opaque(problem.toString)
    }
}
