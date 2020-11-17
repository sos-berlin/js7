package js7.data.order

import io.circe._
import io.circe.syntax._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.ReturnCode
import js7.data.value.{NamedValues, NumericValue}

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
  val succeeded = new Succeeded(NamedValues.empty)
  val succeededRC0 = new Succeeded(Map("returnCode" -> NumericValue(0)))
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

    private[Outcome] sealed trait Companion[A <: Completed] {
      protected def make(namedValues: NamedValues): A

      def apply(returnCode: ReturnCode): A =
        make(NamedValues.rc(returnCode))

      def apply(namedValues: NamedValues = Map.empty): A =
        make(namedValues)
    }

    implicit val jsonCodec = TypedJsonCodec[Completed](
      Subtype(deriveCodec[Failed]),
      Subtype(deriveCodec[Succeeded]))
  }

  final case class Succeeded(namedValues: NamedValues = Map.empty) extends Completed {
    def isSucceeded = true
  }
  object Succeeded extends Completed.Companion[Succeeded]
  {
    protected def make(namedValues: NamedValues): Succeeded =
      if (namedValues.isEmpty)
        succeeded
      else if (namedValues == Map("returnCode" -> NumericValue(0)))
        succeededRC0
      else
        new Succeeded(namedValues)

    implicit val jsonEncoder: Encoder.AsObject[Succeeded] =
      o => JsonObject.fromIterable(
        o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Succeeded] = c =>
      for (namedValues <- c.get[Option[NamedValues]]("namedValues").map(_ getOrElse Map.empty)) yield
        Succeeded(namedValues)
  }

  final case class Failed(errorMessage: Option[String], namedValues: NamedValues)
  extends Completed with NotSucceeded
  {
    def isSucceeded = false
  }
  object Failed extends Completed.Companion[Failed]
  {
    val DefaultErrorMessage = "Failed"

    def apply(errorMessage: Option[String]): Failed =
      Failed(errorMessage, Map.empty)

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
        namedValues <- c.get[Option[NamedValues]]("namedValues").map(_ getOrElse Map.empty)
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

  implicit val jsonCodec = TypedJsonCodec[Outcome](
    Subtype[Succeeded],
    Subtype[Failed],
    Subtype(deriveCodec[Killed]),
    Subtype(deriveCodec[Disrupted]))
}
