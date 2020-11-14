package js7.data.order

import io.circe._
import io.circe.syntax._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.ReturnCode
import js7.data.value.NamedValues

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
  val succeeded = Succeeded(ReturnCode.Success)
  val RecoveryGeneratedOutcome = Disrupted(Disrupted.JobSchedulerRestarted)

  /** The job has terminated. */
  sealed trait Completed extends Outcome {
    def returnCode: ReturnCode
    def namedValues: NamedValues
  }
  object Completed {
    def apply(success: Boolean, returnCode: ReturnCode): Completed =
      apply(success, returnCode, Map.empty)

    def apply(success: Boolean, returnCode: ReturnCode, namedValues: NamedValues): Completed =
      if (success)
        Succeeded(returnCode, namedValues)
      else
        Failed(returnCode, namedValues)

    private[Outcome] sealed trait Companion[A <: Completed] {
      protected def newInstance(returnCode: ReturnCode, namedValues: NamedValues): A

      // re-use memory for usual values.
      private lazy val usualValues: Vector[A] = (0 to 255).map(i => newInstance(ReturnCode(i), Map.empty)).toVector

      def apply(returnCode: ReturnCode, namedValues: NamedValues = Map.empty): A =
        if (namedValues.isEmpty && usualValues.indices.contains(returnCode.number))
          usualValues(returnCode.number)
        else
          newInstance(returnCode, namedValues)
    }

    implicit val jsonCodec = TypedJsonCodec[Completed](
      Subtype(deriveCodec[Failed]),
      Subtype(deriveCodec[Succeeded]))
  }

  final case class Succeeded(returnCode: ReturnCode, namedValues: NamedValues = Map.empty) extends Completed {
    def isSucceeded = true
  }
  object Succeeded extends Completed.Companion[Succeeded]
  {
    def apply(namedValues: NamedValues): Succeeded = apply(ReturnCode.Success, namedValues)

    protected def newInstance(returnCode: ReturnCode, namedValues: NamedValues): Succeeded =
      new Succeeded(returnCode, namedValues)

    implicit val jsonEncoder: Encoder.AsObject[Succeeded] =
      o => JsonObject.fromIterable(
        ("returnCode" -> o.returnCode.asJson) ::
          o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Succeeded] =
      c => for {
        returnCode <- c.get[ReturnCode]("returnCode")
        namedValues <- c.get[Option[NamedValues]]("namedValues").map(_ getOrElse Map.empty)
      } yield Succeeded(returnCode, namedValues)
  }

  final case class Failed(errorMessage: Option[String], returnCode: ReturnCode, namedValues: NamedValues)
  extends Completed with NotSucceeded
  {
    def isSucceeded = false
  }
  object Failed extends Completed.Companion[Failed]
  {
    val DefaultErrorMessage = "Failed"

    def apply(errorMessage: Option[String], returnCode: ReturnCode): Failed =
      Failed(errorMessage, returnCode, Map.empty)

    protected def newInstance(returnCode: ReturnCode, namedValues: NamedValues): Failed =
      Failed(errorMessage = None, returnCode, namedValues)

    implicit val jsonEncoder: Encoder.AsObject[Failed] =
      o => JsonObject.fromIterable(
        ("message" -> o.errorMessage.asJson) ::
        ("returnCode" -> o.returnCode.asJson) ::
        o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Failed] =
      c => for {
        errorMessage <- c.get[Option[String]]("message")
        returnCode <- c.get[ReturnCode]("returnCode")
        namedValues <- c.get[Option[NamedValues]]("namedValues").map(_ getOrElse Map.empty)
      } yield Failed(errorMessage, returnCode, namedValues)
  }

  final case class Cancelled(outcome: Outcome.Completed)
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
    Subtype(deriveCodec[Cancelled]),
    Subtype(deriveCodec[Disrupted]))
}
