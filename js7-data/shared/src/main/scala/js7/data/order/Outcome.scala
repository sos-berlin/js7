package js7.data.order

import io.circe._
import io.circe.syntax._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.process.ReturnCode
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.NamedValues
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
sealed trait Outcome
{
  def isSucceeded: Boolean

  final def isFailed = !isSucceeded
}

object Outcome
{
  val succeeded = Succeeded.empty
  val succeededRC0 = Succeeded.returnCode0
  val failed = new Failed(None, Map.empty)
  val RecoveryGeneratedOutcome = new Disrupted(Disrupted.JobSchedulerRestarted)

  /** The job has terminated. */
  sealed trait Completed extends Outcome {
    def namedValues: NamedValues
  }
  object Completed {
    def apply(success: Boolean): Completed =
      apply(success, Map.empty)

    def apply(success: Boolean, namedValues: NamedValues): Completed =
      if (success)
        Succeeded(namedValues)
      else
        Failed(namedValues)

    def fromChecked(checked: Checked[Outcome.Completed]): Outcome.Completed =
      checked match {
        case Left(problem) => Outcome.Failed.fromProblem(problem)
        case Right(o) => o
      }

    def fromTry(tried: Try[Outcome.Completed]): Completed =
      tried match {
        case Failure(t) => Failed.fromThrowable(t)
        case Success(o) => o
      }

    private[Outcome] sealed trait Companion[A <: Completed] {
      protected def make(namedValues: NamedValues): A

      def rc(returnCode: Int): A =
        make(NamedValues.rc(returnCode))

      def rc(returnCode: ReturnCode): A =
        make(NamedValues.rc(returnCode))

      def apply(namedValues: NamedValues = Map.empty): A =
        make(namedValues)
    }

    implicit val jsonCodec = TypedJsonCodec[Completed](
      Subtype(deriveCodec[Failed]),
      Subtype(deriveCodec[Succeeded]))
  }

  final case class Succeeded(namedValues: NamedValues) extends Completed {
    def isSucceeded = true
  }
  object Succeeded extends Completed.Companion[Succeeded]
  {
    val empty = new Succeeded(Map.empty)
    val returnCode0 = new Succeeded(NamedValues.rc(0))

    protected def make(namedValues: NamedValues): Succeeded =
      if (namedValues.isEmpty)
        empty
      else if (namedValues == returnCode0.namedValues)
        returnCode0
      else
        new Succeeded(namedValues)

    implicit val jsonEncoder: Encoder.AsObject[Succeeded] =
      o => JsonObject.fromIterable(
        o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Succeeded] = c =>
      for (namedValues <- c.getOrElse[NamedValues]("namedValues")(Map.empty)) yield
        make(namedValues)
  }

  final case class Failed(errorMessage: Option[String], namedValues: NamedValues/*TODO DELETE THIS?*/)
  extends Completed with NotSucceeded
  {
    def isSucceeded = false
  }
  object Failed extends Completed.Companion[Failed]
  {
    val DefaultErrorMessage = "Failed"

    def apply(errorMessage: Option[String]): Failed =
      Failed(errorMessage, Map.empty)

    def fromProblem(problem: Problem): Failed =
      Failed(Some(problem.toString))

    def fromThrowable(throwable: Throwable): Failed =
      Failed(Some(throwable.toStringWithCauses))

    protected def make(namedValues: NamedValues): Failed = {
      if (namedValues.isEmpty)
        failed
      else
        Failed(errorMessage = None, namedValues)
    }

    implicit val jsonEncoder: Encoder.AsObject[Failed] =
      o => JsonObject.fromIterable(
        ("message" -> o.errorMessage.asJson) ::
        o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Failed] =
      c => for {
        errorMessage <- c.get[Option[String]]("message")
        namedValues <- c.getOrElse[NamedValues]("namedValues")(Map.empty)
      } yield Failed(errorMessage, namedValues)
  }

  final case class Killed(outcome: Outcome.Completed)
  extends Outcome {
    def isSucceeded = false
  }

  /** No response from job - some other error has occurred. */
  final case class Disrupted(reason: Disrupted.Reason) extends Outcome with NotSucceeded {
    def isSucceeded = false
  }
  object Disrupted {
    def apply(problem: Problem): Disrupted =
      Disrupted(Other(problem))

    sealed trait Reason {
      def problem: Problem
    }

    final case object JobSchedulerRestarted extends Reason {
      val problem = Problem.pure("JS7 stopped while order was in-process")
    }

    final case class Other(problem: Problem) extends Reason

    object Reason {
      implicit val jsonCodec = TypedJsonCodec[Reason](
        Subtype(JobSchedulerRestarted),
        Subtype(deriveCodec[Other]))
    }
  }

  sealed trait NotSucceeded extends Outcome
  object NotSucceeded {
    implicit val jsonCodec = TypedJsonCodec[NotSucceeded](
      Subtype[Failed],
      Subtype(deriveCodec[Disrupted]))
  }

  private val typedJsonCodec = TypedJsonCodec[Outcome](
    Subtype[Succeeded],
    Subtype[Failed],
    Subtype(deriveCodec[Killed]),
    Subtype(deriveCodec[Disrupted]))

  private val predefinedRC0SucceededJson: JsonObject =
    TypedJsonCodec.typeField[Succeeded] +: Succeeded.returnCode0.asJsonObject

  implicit val jsonEncoder: Encoder.AsObject[Outcome] = outcome =>
    if (outcome eq Succeeded.returnCode0)
      predefinedRC0SucceededJson
    else
      typedJsonCodec.encodeObject(outcome)

  implicit val jsonDecoder: Decoder[Outcome] =
    typedJsonCodec
}
