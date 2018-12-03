package com.sos.jobscheduler.base.problem

import cats.data.Validated.Invalid
import cats.syntax.semigroup._
import cats.{Eq, Semigroup}
import com.sos.jobscheduler.base.problem.Problem._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.ScalazStyle._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}
import scala.collection.immutable.{Iterable, Seq}
import scala.language.implicitConversions
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
sealed trait Problem
{
  def code: ProblemCode = ProblemCode.empty

  def throwable: Throwable

  def throwableOption: Option[Throwable]

  def cause: Option[Problem]

  def head: Problem = this

  def withKey(key: Any): Problem = withPrefix(s"Problem with '$key':")

  def withPrefix(prefix: String): Problem = Problem(prefix) |+| this

  final def wrapProblemWith(message: String) = new Lazy(message, Some(this))

  override def equals(o: Any) = o match {
    case o: HasCode ⇒ false
    case o: Problem ⇒ toString == o.toString
    case _ ⇒ false
  }

  override def hashCode = toString.hashCode

  /** Message with cause. **/
  override final def toString = message + cause.fold("")(o ⇒ s" [$o]")

  /** Message without cause. **/
  protected[problem] def message: String
}

object Problem
{
  implicit def toInvalid[A](problem: Problem): Invalid[Problem] =
    Invalid(problem)

  def apply(messageFunction: ⇒ String): Problem =
    new Lazy(messageFunction)

  def apply(code: ProblemCode, insertions: Any*)(implicit codeToString: ProblemCode ⇒ Option[String]): Problem =
    DynamicMessage(code, insertions.toVector)(codeToString)

  def eager(message: String): Problem =
    apply(message)

  def eager(throwable: Throwable): Problem =
    new FromEagerThrowable(throwable)

  def fromLazyThrowable(throwable: ⇒ Throwable): Problem =
    new FromLazyThrowable(() ⇒ throwable)

  private[Problem] trait Simple extends Problem {
    protected def rawMessage: String
    final def throwableOption: None.type = None

    // A val, to compute message only once.
    final lazy val message =
      rawMessage match {
        case null ⇒ "A problem occurred (null)"
        case "" ⇒ "A problem occurred (no message)"
        case o ⇒ o
      }

    final def throwable =
      cause match {
        case Some(p: FromEagerThrowable) ⇒ new ProblemException(this, p.throwable)
        case _ ⇒ new ProblemException(this)
      }

    override final def withPrefix(prefix: String) = new Lazy(normalizePrefix(prefix) + toString)
  }

  private sealed trait HasCode extends Simple {
    def insertions: Seq[Any]

    override def equals(o: Any) = o match {
      case o: HasCode ⇒ code == o.code && insertions == o.insertions && cause == o.cause
      case _ ⇒ false
    }
  }

  private case class StaticMessage private[Problem](
    override val code: ProblemCode,
    insertions: Seq[Any],
    rawMessage: String,
    cause: Option[Problem] = None)
  extends HasCode

  private case class DynamicMessage private[Problem](
    override val code: ProblemCode,
    insertions: Seq[Any],
    originalMessage: Option[String] = None,
    cause: Option[Problem] = None)
    (implicit codeToString: ProblemCode ⇒ Option[String])
  extends HasCode {
    protected def rawMessage = {
      val used = scala.collection.mutable.BitSet()
      val message =
        codeToString(code) match {
          case Some(pattern) ⇒
            val it = pattern.iterator.buffered
            val sb = new StringBuilder(pattern.length)
            while (it.hasNext) {
              val c = it.next()
              if (c == '$' && it.headOption.exists(c ⇒ c >= '1' && c <= '9')) {
                val n = it.next() - '1'
                if (n >= insertions.length) {
                  sb ++= "[]"
                } else {
                  sb ++= insertions(n).toString
                  used += n
                }
              } else
                sb += c
            }
            sb.toString
          case None ⇒ code.string
        }
      val unused = insertions.zipWithIndex.collect {
        case (s, i) if !used(i) ⇒ s
      }
      message + (if (unused.isEmpty) "" else unused.mkString(" [", ", ", "]"))
    }

    override def equals(o: Any) = o match {
      case o: HasCode ⇒ code == o.code && insertions == o.insertions && cause == o.cause
      case _ ⇒ false
    }
  }

  class Eager protected[problem](protected val rawMessage: String, val cause: Option[Problem] = None) extends Simple {
    override final def hashCode = super.hashCode  // Derived case class should not override
  }

  class Lazy protected[problem](messageFunction: ⇒ String, val cause: Option[Problem] = None) extends Simple {
    protected def rawMessage = messageFunction
    override final def hashCode = super.hashCode  // Derived case class should not override
  }

  def set(problems: String*): Multiple =
    multiple(problems.toSet)

  def multiple(problems: String*): Multiple =
    multiple(problems.toImmutableSeq)

  def multiple(problems: Iterable[String]): Multiple =
    Multiple(problems.map(o ⇒ new Lazy(o)))

  final case class Multiple private[problem](problems: Iterable[Problem]) extends Problem {
    require(problems.nonEmpty)

    def throwable = new ProblemException(this)

    def throwableOption: None.type = None

    lazy val message = problems map (_.toString) reduce combineMessages

    override def head = problems.head

    def cause = None

    override def equals(o: Any) = o match {
      case o: Multiple ⇒
        (problems, o.problems) match {
          case (problems: Set[Problem], _) ⇒ problems == o.problems.toSet  // Ignore ordering (used in tests)
          case (_, o: Set[Problem])        ⇒ problems.toSet == o           // Ignore ordering (used in tests)
          case _                           ⇒ problems == o.problems
        }
        case _ ⇒ super.equals(o)
      }

    override def hashCode = problems.map(_.hashCode).sum  // Ignore ordering (used in tests)
  }

  sealed trait FromThrowable extends Problem

  private final class FromEagerThrowable(val throwable: Throwable) extends FromThrowable
  {
    lazy val message = throwable.toStringWithCauses

    def throwableOption: Some[Throwable] =
      Some(throwable)

    def cause: None.type =
      None
  }

  private final class FromLazyThrowable(throwableFunction: () ⇒ Throwable) extends FromThrowable
  {
    private lazy val throwable_ = throwableFunction()
    lazy val message = throwable_.toStringWithCauses

    def throwable = throwable_

    def throwableOption: Some[Throwable] =
      Some(throwable_)

    def cause: None.type =
      None
  }

  implicit val semigroup: Semigroup[Problem] = {
    case (a: Problem, b: FromThrowable) ⇒
      Problem.fromLazyThrowable(new ProblemException(a, b.throwable) with NoStackTrace)

    case (a: FromThrowable, b: Problem) ⇒
      Problem.fromLazyThrowable {
        val t = new ProblemException(a, b.throwable) with NoStackTrace
        t.setStackTrace(a.throwable.getStackTrace)
        t
      }

    case (a: Simple, b: Simple) ⇒
      Multiple(a :: b :: Nil)

    case (a: Simple, b: Multiple) ⇒
      Multiple(a +: b.problems.toVector)

    case (a: Multiple, b: Lazy) ⇒
      Multiple(a.problems.toVector :+ b)

    case (a: Multiple, b: Problem) ⇒
      Multiple(a.problems.toVector :+ new Lazy(b.toString))  // TODO If b is FromThrowable, then b.throwable is lost
  }

  implicit val eqv: Eq[Problem] = Eq.fromUniversalEquals[Problem]

  private def combineMessages(a: String, b: String) =
    if (b.trim.isEmpty)
      a
    else
      normalizePrefix(a) + b

  private def normalizePrefix(prefix: String): String =
    if (prefix matches ".*[:-] *")
      if (prefix endsWith " ") prefix else prefix + " "
    else if (prefix.trim.isEmpty && prefix.endsWith(" "))
      prefix
    else
      prefix + "\n & "

  implicit val jsonEncoder: ObjectEncoder[Problem] = _ match {
    case problem: HasCode ⇒
      JsonObject(
        "code" → problem.code.asJson,
        "insertions" → (problem.insertions.nonEmpty ? problem.insertions.map(_.toString)).asJson,
        "message" → Json.fromString(problem.toString))  // Not value.message, JSON differs from Scala

    case problem: Problem ⇒
      JsonObject(
        "message" → Json.fromString(problem.toString))  // Not value.message, JSON differs from Scala
  }

  val typedJsonEncoder: ObjectEncoder[Problem] = {
    val typeField = "TYPE" → Json.fromString("Problem")
    problem ⇒ typeField +: jsonEncoder.encodeObject(problem)
  }

  implicit val jsonDecoder: Decoder[Problem] =
    c ⇒ for {
      maybeCode ← c.get[Option[ProblemCode]]("code")
      insertions ← c.get[Option[Seq[String]]]("insertions") map (_ getOrElse Nil)
      message ← c.get[String]("message")
    } yield
      maybeCode match {
        case None ⇒ Problem.eager(message)
        case Some(code) ⇒ StaticMessage(code, insertions, message)
      }
}
