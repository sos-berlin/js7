package js7.base.problem

import cats.syntax.semigroup.*
import cats.{Eq, Semigroup}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import javax.annotation.Nullable
import js7.base.annotation.javaApi
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import scala.collection.immutable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
sealed trait Problem
{
  def maybeCode: Option[ProblemCode] = None

  @javaApi @Nullable
  final def codeOrNull: ProblemCode =
    maybeCode.orNull

  def throwable: Throwable

  def throwableOption: Option[Throwable]

  /** For Java, return an unchecked Exception. */
  final def toRuntimeException: RuntimeException =
    throwable match {
      case t: RuntimeException => t
      case t => new RuntimeException(t.toString, t)
    }

  def cause: Option[Problem]

  def head: Problem = this

  final def withKey(key: Any): Problem = withPrefix(s"Problem with '$key':")

  final def withPrefix(prefix: String): Problem =
    Problem.pure(prefix) |+| this

  final def is(companion: Problem.Coded.Companion): Boolean =
    maybeCode contains companion.code

  final def wrapProblemWith(message: String) = new Problem.Lazy(message, Some(this))

  override def equals(o: Any) = o match {
    case _: Problem.HasCode => false
    case o: Problem => toString == o.toString
    case _ => false
  }

  override def hashCode = toString.hashCode

  /** Message with cause. **/
  override def toString = messageWithCause

  final def messageWithCause = message + cause.fold("")(o => s" [$o]")

  /** Message without cause. **/
  protected[problem] def message: String

  def httpStatusCode: Int = Problem.DefaultHttpStatusCode
}

object Problem extends Semigroup[Problem]
{
  private val DefaultHttpStatusCode = 400  // Bad Request

  @javaApi
  def singleton = this

  implicit def toInvalid[A](problem: Problem): Left[Problem, A] =
    Left(problem)

  def apply(messageFunction: => String, cause: Option[Problem] = None): Problem =
    lzy(messageFunction, cause)

  def lzy(messageFunction: => String, cause: Option[Problem] = None): Problem =
    new Lazy(messageFunction, cause)

  def pure(message: String): Problem =
    apply(message, cause = None)

  def withHttpStatus(message: String, throwable: Throwable, httpStatusCode: Int): Problem = {
    val msg = message
    new FromEagerThrowable(throwable, httpStatusCode = httpStatusCode) {
      override lazy val message = msg
    }
  }

  def pure(message: String, cause: Option[Problem]): Problem =
    apply(message, cause)

  @javaApi
  def of(message: String): Problem =
    pure(message)

  def fromThrowable(throwable: Throwable): Problem =
    new FromEagerThrowable(throwable)

  def fromLazyThrowable(throwable: => Throwable): Problem =
    new FromLazyThrowable(() => throwable)

  implicit val problemEq: Eq[Problem] = Eq.fromUniversalEquals[Problem]

  implicit val problemSemigroup: Semigroup[Problem] = this

  def combine(a: Problem, b: Problem) = (a, b) match {
    case (a: Combined, b: Combined) =>
      Combined(a.problems.toVector ++ b.problems)

    case (a: Combined, b: Problem) =>
      Combined(a.problems.toVector :+ b)

    case (a: Problem, b: Combined) =>
      Combined(a +: b.problems.toVector)

    case (a: Problem, b: Problem) =>
      Combined(Vector(a, b))
  }

  def combineToChecked(problems: IterableOnce[Problem]): Checked[Unit] =
    combineAllOption(problems).toLeft(())

  private def combineMessages(a: String, b: String) = {
    val b1 = b.trim
    if (b1.isEmpty || a.trim == b1)
      a
    else
      normalizePrefix(a) + b
  }

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
        case Some(p: FromEagerThrowable) => new ProblemException.NoStackTrace(this, p.throwable)
        case _ => new ProblemException.NoStackTrace(this)
      }
  }

  trait HasCode extends Simple {
    def code: ProblemCode
    def arguments: Map[String, String]

    override final def maybeCode = Some(code)

    final def cause = None

    def rawMessage = CodedMessages.problemCodeToMessage(code, arguments)

    override def equals(o: Any) = o match {
      case o: HasCode => code == o.code && arguments == o.arguments && cause == o.cause
      case _ => false
    }

    override def hashCode = (code.hashCode * 31 + arguments.hashCode) * 31 + cause.hashCode

    override def toString = {
      val msg = messageWithCause
      if (s"$msg ".startsWith(s"${code.string} ") ||
          s"$msg(".startsWith(s"${code.string}(")) msg
      else code.string + ": " + msg
    }
  }
  object HasCode {
    def apply(code: ProblemCode, arguments: Map[String, String]) = {
      val c = code
      val a = arguments
      new HasCode {
        val code = c
        val arguments = a
      }
    }
    def unapply(coded: HasCode) = Some((coded.code, coded.arguments))
  }

  trait Coded extends HasCode {
    val code = Coded.codeOf(getClass)
    def toSerialized: Problem = HasCodeAndMessage(code, arguments, message)
  }
  object Coded {
    trait Companion {
      val code = codeOf(getClass)
    }
    private[problem] def codeOf(clas: Class[?]) =
      ProblemCode(clas.simpleScalaName stripSuffix "Problem")
  }

  trait ArgumentlessCoded extends Coded with Coded.Companion {
    override val code = Coded.codeOf(getClass)
    final def arguments = Map.empty
  }

  private[problem] case class HasCodeAndMessage(
    code: ProblemCode,
    arguments: Map[String, String],
    override val rawMessage: String)
  extends HasCode

  class Eager(protected val rawMessage: String, val cause: Option[Problem] = None)
  extends Simple {
    override final def hashCode = super.hashCode  // Derived case class should not override
  }

  class Lazy(messageFunction: => String, val cause: Option[Problem] = None)
  extends Simple {
    protected def rawMessage = messageFunction
    override final def hashCode = super.hashCode  // Derived case class should not override
  }

  def combined(problems: immutable.Iterable[String]): Combined =
    Combined(problems.map(o => new Lazy(o)))

  final case class Combined(problems: immutable.Iterable[Problem]) extends Problem {
    require(problems.nonEmpty)

    def throwable = throwableOption getOrElse new ProblemException.NoStackTrace(this)

    def throwableOption = {
      val throwables = problems.flatMap(_.throwableOption)
      throwables.headOption.map { head =>
        val throwable = new ProblemException(this)
        throwable.setStackTrace(head.getStackTrace)
        for (o <- throwables.tail) throwable.appendStackTrace(o.getStackTrace)
        throwable
      }
    }

    lazy val message = problems.map(_.toString) reduce combineMessages

    override def head = problems.head

    def cause = None

    override def equals(o: Any) = o match {
      case o: Combined =>
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

  private class FromEagerThrowable(
    val throwable: Throwable,
    override val httpStatusCode: Int = DefaultHttpStatusCode)
  extends FromThrowable
  {
    lazy val message = throwable.toStringWithCauses

    def throwableOption: Some[Throwable] =
      Some(throwable)

    def cause: None.type =
      None
  }

  private final class FromLazyThrowable(throwableFunction: () => Throwable)
  extends FromThrowable
  {
    private lazy val throwable_ = throwableFunction()
    lazy val message = throwable_.toStringWithCauses

    def throwable = throwable_

    def throwableOption: Some[Throwable] =
      Some(throwable_)

    def cause: None.type =
      None
  }

  private def normalizePrefix(prefix: String): String =
    if (prefix matches ".*[:-] *")
      if (prefix endsWith " ") prefix else prefix + " "
    else if (prefix.trim.isEmpty && prefix.endsWith(" "))
      prefix
    else
      prefix + ";\n"

  implicit val jsonEncoder: Encoder.AsObject[Problem] = problem =>
    JsonObject.fromIterable(
      (problem match {
        case problem: HasCode => Seq(
          ("code" -> problem.code.asJson),
          ("arguments" -> problem.arguments.??.asJson))

        case _: Problem =>
          Seq.empty
      }) :+
        ("message" -> Json.fromString(problem.messageWithCause/*Not value.message, JSON differs from Scala*/)))

  val typedJsonEncoder: Encoder.AsObject[Problem] = {
    val typeField = "TYPE" -> Json.fromString("Problem")
    problem => typeField +: jsonEncoder.encodeObject(problem)
  }

  implicit val jsonDecoder: Decoder[Problem] =
    c => for {
      maybeCode <- c.get[Option[ProblemCode]]("code")
      arguments <- c.getOrElse[Map[String, String]]("arguments")(Map.empty)
      message <- c.get[String]("message")
    } yield
      maybeCode match {
        case None => Problem.pure(message)
        case Some(code) => HasCodeAndMessage(code, arguments, message)
      }

  object IsThrowable {
    def unapply(problem: Problem): Option[Throwable] =
      problem.throwableOption
  }
}
