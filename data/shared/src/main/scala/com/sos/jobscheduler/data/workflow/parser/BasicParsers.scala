package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.Identifier.{isIdentifierPart, isIdentifierStart}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import fastparse.all._
import fastparse.core
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
private[parser] object BasicParsers
{
  object ops {
    implicit final class RichParserApi[T](private val underlying: Parser[T]) extends AnyVal {
      /**
       * Parses using this followed by whitespace `w` and the parser `p`
       */
      def ~~[V, R](p: Parser[V])(implicit ev: core.Implicits.Sequencer[T, V, R]): Parser[R] =
        underlying ~ w ~ p

      /**
       * Parses using this followed by whitespace `w` (cuts here) and the parser `p`
       */
      def ~~/[V, R](p: Parser[V])(implicit ev: core.Implicits.Sequencer[T, V, R]): Parser[R] =
        underlying ~ w ~/ p
    }
  }

  import ops.RichParserApi

  private val inlineComment = {
    val untilStar = P(CharsWhile(_ != '*', min = 0) ~ "*")
    P("/*" ~ untilStar ~ (!"/" ~ untilStar).rep ~ "/")
  }
  private val lineEndComment = P("//" ~ CharsWhile(_ != '\n'))
  private val comment = P(inlineComment | lineEndComment)
  /** Optional whitespace including line ends */
  val w = P((CharsWhileIn(" \t\r\n") | comment).rep)
  /** Optional horizontal whitespace */
  val h = P((CharsWhileIn(" \t") | comment).rep)
  val comma = w ~ "," ~ w
  //val newline = P(h ~ "\r".? ~ "\n" ~ w)
  //val commaOrNewLine = P(h ~ ("," | (newline ~ w ~ ",".?)) ~ w)
  val int = P[Int](("-".? ~ CharsWhile(c ⇒ c >= '0' && c <= '9')).! map (_.toInt))
  val identifierEnd = &(CharPred(c ⇒ !isIdentifierPart(c))) | End
  val identifier = P[String]((CharPred(isIdentifierStart) ~ CharsWhile(isIdentifierPart, min = 0)).! ~ identifierEnd)
  val quotedString = P[String] {
    val singleQuoted = P("'" ~/
      (CharsWhile(ch ⇒ ch != '\'' && ch >= ' ', min = 0).! ~ "'").opaque(
        "Single-quoted (') string is not properly terminated or contains a non-printable character"))
    val doubleQuoted = P("\"" ~/
      (CharsWhile(ch ⇒ ch != '"' && ch >= ' ' && ch != '\\', min = 0).! ~ "\"").opaque(
        "Double-quoted (\") string is not properly terminated or contains a non-printable character or backslash (\\)"))
      .flatMap {
        case o if o contains '$' ⇒ invalid("Variable interpolation via '$' in double-quoted is not implemented. Consider using single quotes (')")
        case o ⇒ valid(o)
      }
    singleQuoted | doubleQuoted
  }
  //val javaClassName = P((identifier ~ ("." ~ identifier).rep).!)

  val pathString = P[String](quotedString)

  def keyword = identifier

  def keyword(name: String) = P[Unit](name ~ identifierEnd)

  def path[P <: TypedPath: TypedPath.Companion] = P[P](
    pathString map (p ⇒ FolderPath.Root.resolve[P](p)))

  def keyValueMap[A](keyParsers: Map[String, P[A]]): P[KeyToValue[A]] =
    keyValues(keyParsers)
      .flatMap(kvs ⇒
        kvs.duplicateKeys(_._1) match {
          case Some(dups) ⇒ Fail.opaque("Duplicate keywords: " + dups.keys.mkString(", "))
          case None       ⇒ PassWith(KeyToValue(kvs.toMap))
        })

  final case class KeyToValue[A](keyToValue: Map[String, A]) {
    def apply[A1 <: A](key: String, default: ⇒ A1): P[A1] =
      PassWith(keyToValue.get(key).fold(default)(_.asInstanceOf[A1]))

    def apply[A1 <: A](key: String): P[A1] =
      keyToValue.get(key) match {
        case None    ⇒ Fail.opaque(s"Missing required argument '$key='")
        case Some(o) ⇒ PassWith(o.asInstanceOf[A1])
      }

    def get[A1 <: A](key: String): P[Option[A1]] =
      PassWith(keyToValue.get(key) map (_.asInstanceOf[A1]))

    def noneOrOneOf[A1 <: A](keys: Set[String]): P[Option[(String, A1)]] = {
      val intersection = keyToValue.keySet & keys
      intersection.size match {
        case 0 ⇒ PassWith(None)
        case 1 ⇒ PassWith(Some(intersection.head → keyToValue(intersection.head).asInstanceOf[A1]))
        case _ ⇒ Fail.opaque(s"Contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOf[A1 <: A](keys: Set[String]): P[(String, A1)] = {
      val intersection = keyToValue.keySet & keys
      intersection.size match {
        case 0 ⇒ Fail.opaque("Missing one of the keywords: " + keys.mkString(", "))
        case 1 ⇒ PassWith(intersection.head → keyToValue(intersection.head).asInstanceOf[A1])
        case _ ⇒ Fail.opaque(s"Contradicting keywords: ${intersection.mkString("; ")}")
      }
    }

    def oneOfOr[A1 <: A](keys: Set[String], default: A1): P[A1] = {
      val intersection = keyToValue.keySet & keys
      intersection.size match {
        case 0 ⇒ PassWith(default)
        case 1 ⇒ PassWith(keyToValue(intersection.head).asInstanceOf[A1])
        case _ ⇒ Fail.opaque(s"Contradicting keywords: ${intersection.mkString("; ")}")
      }
    }
  }

  def keyValues[A](keyParsers: Map[String, P[A]]): P[Seq[(String, A)]] =
    commaSeq(
      keyParsers
        .map { case (k, p) ⇒ keyValue(k, p) map k.→ }
        .reduce((a, b) ⇒ a | b))

  def keyValue[V](name: String, valueParser: Parser[V]): Parser[V] =
    P(name ~ h ~ "=" ~~/ valueParser)

  def curly[A](parser: Parser[A]): Parser[A] =
    P(h ~ "{" ~~/ parser ~~ "}")

  def inParentheses[A](parser: Parser[A]): Parser[A] =
    P(h ~ "(" ~~/ parser ~~ ")")

  //def parenthesizedCommaSeq[A](parser: Parser[A]): Parser[collection.Seq[A]] =
  //  P("(") ~~/
  //    (P(")").map(_ ⇒ Nil) | commaSeq(parser) ~~ ")")

  def bracketCommaSeq[A](parser: Parser[A]): Parser[collection.Seq[A]] =
    P("[") ~~/
      (P("]").map(_ ⇒ Nil) | commaSeq(parser) ~~ "]")

  def commaSeq[A](parser: Parser[A]): Parser[collection.Seq[A]] =
    P(parser ~ (comma ~ parser).rep ~ w) map {
      case (head, tail) ⇒ head +: tail
    }

  def leftRecurse[A, O](operand: P[A], operator: P[O])(operation: (A, O, A) ⇒ P[A]): P[A] = {
    operand ~/ (w ~ operator ~~/ operand).rep(min = 0) flatMap {
      case (head, tail) ⇒
        def loop(left: A, tail: List[(O, A)]): P[A] =
          tail match {
            case Nil ⇒
              valid(left)
            case (op, right) :: tl ⇒
              operation(left, op, right) flatMap (a ⇒ loop(a, tl))
          }
        loop(head, tail.toList)
    }
  }

  def newInstance[A: ClassTag](name: String): A =
    loadClass[A](name).newInstance()

  def loadClass[A: ClassTag](name: String): Class[A] = {
    val c = Class.forName(name, false, Thread.currentThread.getContextClassLoader).asInstanceOf[Class[A]]
    require(implicitClass[A] isAssignableFrom c, s"Class $name does not implement ${implicitClass[A].getName}")
    c
  }

  def valid[A](a: A): Parser[A] = CheckedParser(Valid(a))

  def invalid(message: String): Parser[Nothing] = CheckedParser(Problem.fromEager(message))

  final case class CheckedParser[T](checked: Checked[T]) extends core.Parser[T, Char, String]{
    def parseRec(cfg: core.ParseCtx[Char, String], index: Int) =
      checked match {
        case Valid(elem) ⇒ success(cfg.success, elem, index, Set.empty, false)
        case Invalid(problem) ⇒
          val failure = fail(cfg.failure, index)
          failure.lastParser = ProblemParser(problem)
          failure
      }

    override val toString = "CheckedParser"
  }

  final case class ProblemParser(problem: Problem) extends core.Parser[Nothing, Char, String] {
    def parseRec(cfg: core.ParseCtx[Char, String], index: Int) = throw new NotImplementedError(s"ProblemParser($toString)")
    override def toString = problem.toString
  }
}
