package com.sos.jobscheduler.base.problem

import cats.data.Validated.Invalid
import cats.syntax.semigroup._
import cats.{Eq, Semigroup}
import com.sos.jobscheduler.base.problem.Problem._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichThrowable}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}
import scala.collection.immutable.Iterable
import scala.language.implicitConversions
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
sealed trait Problem
{
  def codeOption: Option[ProblemCode] = None

  def throwable: Throwable

  def throwableOption: Option[Throwable]

  def cause: Option[Problem]

  def head: Problem = this

  def withKey(key: Any): Problem = withPrefix(s"Problem with '$key':")

  def withPrefix(prefix: String): Problem = Problem(prefix) |+| this

  final def wrapProblemWith(message: String) = new Lazy(message, Some(this))

  override def equals(o: Any) = o match {
    case _: HasCode => false
    case o: Problem => toString == o.toString
    case _ => false
  }

  override def hashCode = toString.hashCode

  /** Message with cause. **/
  override def toString = messageWithCause

  final def messageWithCause = message + cause.fold("")(o => s" [$o]")

  /** Message without cause. **/
  protected[problem] def message: String
}

object Problem
{
  implicit def toInvalid[A](problem: Problem): Invalid[Problem] =
    Invalid(problem)

  def apply(messageFunction: => String): Problem =
    new Lazy(messageFunction)

  def pure(message: String): Problem =
    apply(message)

  def pure(throwable: Throwable): Problem =
    new FromEagerThrowable(throwable)

  def fromLazyThrowable(throwable: => Throwable): Problem =
    new FromLazyThrowable(() => throwable)

  private[Problem] trait Simple extends Problem {
    protected def rawMessage: String
    final def throwableOption: None.type = None

    // A val, to compute message only once.
    final lazy val message =
      rawMessage match {
        case null => "A problem occurred (null)"
        case "" => "A problem occurred (no message)"
        case o => o
      }

    final def throwable =
      cause match {
        case Some(p: FromEagerThrowable) => new ProblemException(this, p.throwable)
        case _ => new ProblemException(this)
      }

    override final def withPrefix(prefix: String) = new Lazy(normalizePrefix(prefix) + toString)
  }

  trait HasCode extends Simple {
    def code: ProblemCode
    def arguments: Map[String, String]

    override final def codeOption = Some(code)

    final def cause = None

    def rawMessage = CodedMessages.problemCodeToMessage(code, arguments)

    override def equals(o: Any) = o match {
      case o: HasCode => code == o.code && arguments == o.arguments && cause == o.cause
      case _ => false
    }

    override def toString = {
      val msg = messageWithCause
      if (msg + " " startsWith code.string + " ") msg
      else code.string + ": " + msg
    }
  }
  object HasCode {
    def unapply(coded: HasCode) = Some((coded.code, coded.arguments))
  }

  trait Coded extends HasCode {
    final val code = Coded.codeOf(getClass)
  }
  object Coded {
    trait Companion {
      val code = codeOf(getClass)
    }
    private[Coded] def codeOf(clas: Class[_]) = ProblemCode(clas.simpleScalaName stripSuffix "Problem")
  }

  trait ArgumentlessCoded extends Coded {
    final def arguments = Map.empty
  }

  private case class StaticMessage private[Problem](
    code: ProblemCode,
    arguments: Map[String, String],
    override val rawMessage: String)
  extends HasCode

  class Eager protected[problem](protected val rawMessage: String, val cause: Option[Problem] = None) extends Simple {
    override final def hashCode = super.hashCode  // Derived case class should not override
  }

  class Lazy protected[problem](messageFunction: => String, val cause: Option[Problem] = None) extends Simple {
    protected def rawMessage = messageFunction
    override final def hashCode = super.hashCode  // Derived case class should not override
  }

  def set(problems: String*): Multiple =
    multiple(problems.toSet)

  def multiple(problems: String*): Multiple =
    multiple(problems.toImmutableSeq)

  def multiple(problems: Iterable[String]): Multiple =
    Multiple(problems.map(o => new Lazy(o)))

  final case class Multiple private[problem](problems: Iterable[Problem]) extends Problem {
    require(problems.nonEmpty)

    def throwable = new ProblemException(this) with NoStackTrace

    def throwableOption: None.type = None

    lazy val message = problems map (_.toString) reduce combineMessages

    override def head = problems.head

    def cause = None

    override def equals(o: Any) = o match {
      case o: Multiple =>
        (problems, o.problems) match {
          case (problems: Set[Problem], _) => problems == o.problems.toSet  // Ignore ordering (used in tests)
          case (_, o: Set[Problem])        => problems.toSet == o           // Ignore ordering (used in tests)
          case _                           => problems == o.problems
        }
        case _ => super.equals(o)
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

  private final class FromLazyThrowable(throwableFunction: () => Throwable) extends FromThrowable
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
    case (a: Problem, b: FromThrowable) =>
      Problem.fromLazyThrowable(new ProblemException(a, b.throwable) with NoStackTrace)

    case (a: FromThrowable, b: Problem) =>
      Problem.fromLazyThrowable {
        val t = new ProblemException(a, b.throwable) with NoStackTrace
        t.setStackTrace(a.throwable.getStackTrace)
        t
      }

    case (a: Simple, b: Simple) =>
      Multiple(a :: b :: Nil)

    case (a: Simple, b: Multiple) =>
      Multiple(a +: b.problems.toVector)

    case (a: Multiple, b: Lazy) =>
      Multiple(a.problems.toVector :+ b)

    case (a: Multiple, b: Problem) =>
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

  implicit val jsonEncoder: ObjectEncoder[Problem] = problem =>
    JsonObject.fromIterable(
      ("message" -> Json.fromString(problem.messageWithCause/*Not value.message, JSON differs from Scala*/)) :: (
        problem match {
          case problem: HasCode =>
            ("code" -> problem.code.asJson) ::
            ("arguments" -> (problem.arguments.nonEmpty ? problem.arguments).asJson) ::
            Nil

          case _: Problem =>
            Nil
        }))

  val typedJsonEncoder: ObjectEncoder[Problem] = {
    val typeField = "TYPE" -> Json.fromString("Problem")
    problem => typeField +: jsonEncoder.encodeObject(problem)
  }

  implicit val jsonDecoder: Decoder[Problem] =
    c => for {
      maybeCode <- c.get[Option[ProblemCode]]("code")
      arguments <- c.get[Option[Map[String, String]]]("arguments") map (_ getOrElse Map.empty)
      message <- c.get[String]("message")
    } yield
      maybeCode match {
        case None => Problem.pure(message)
        case Some(code) => StaticMessage(code, arguments, message)
      }
}
