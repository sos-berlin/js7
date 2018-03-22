package com.sos.jobscheduler.core.workflow.notation

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
private[notation] object BasicParsers
{
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
  val quotedString = P("\"" ~ CharsWhile(c ⇒ c != '"' && c != '\\').! ~ "\"")
  //val javaClassName = P((identifier ~ ("." ~ identifier).rep).!)

  val pathString = P[String](quotedString)
  //val pathString = P(("/" ~ identifier ~ ("/" ~ identifier).rep).!)

  def path[P <: TypedPath: TypedPath.Companion] = P[P](
    pathString map (p ⇒ FolderPath.Root.resolve[P](p)))

  def keyValue[V](name: String, valueParser: Parser[V]): Parser[V] =
    P(name ~ h ~ "=" ~ w ~ valueParser)

  def inParentheses[A](parser: Parser[A]): Parser[A] =
    P(h ~ "(" ~ w ~ parser ~ w ~ ")")

  def sequence[A](parser: Parser[A]): Parser[collection.Seq[A]] =
    P("(" ~ commaSeq(parser) ~ ")")

  def commaSeq[A](parser: Parser[A]): Parser[collection.Seq[A]] =
    P(w ~ parser ~ (comma ~ parser).rep ~ w) map {
      case (head, tail) ⇒ head +: tail
    }

  def newInstance[A: ClassTag](name: String): A =
    loadClass[A](name).newInstance()

  def loadClass[A: ClassTag](name: String): Class[A] = {
    val c = Class.forName(name, false, Thread.currentThread.getContextClassLoader).asInstanceOf[Class[A]]
    require(implicitClass[A] isAssignableFrom c, s"Class $name does not implement ${implicitClass[A].getName}")
    c
  }

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
