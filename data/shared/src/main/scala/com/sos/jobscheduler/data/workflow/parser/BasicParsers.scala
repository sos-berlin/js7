package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
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
  //Scala-like: val instructionTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))
  val identifier = P((CharPred(isIdentifierStart) ~ CharsWhile(isIdentifierPart, min = 0)).!)
  def keyword(name: String) = P[Unit](name)  // TODO require word boundary
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

  def path[P <: TypedPath: TypedPath.Companion] = P[P](
    pathString map (p ⇒ FolderPath.Root.resolve[P](p)))

  def keyValue[V](name: String, valueParser: Parser[V]): Parser[V] =
    P(name ~ h ~ "=" ~~ valueParser)

  def inParentheses[A](parser: Parser[A]): Parser[A] =
    P(h ~ "(" ~~/ parser ~~ ")")

  def parenthesizedCommaSeq[A](parser: Parser[A]): Parser[collection.Seq[A]] =
    P("(") ~~/ commaSeq(parser) ~~ ")"

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
