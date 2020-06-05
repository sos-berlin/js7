package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.ScalazStyle._
import js7.data.job.ReturnCode
import io.circe._
import io.circe.syntax._

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
  sealed trait Undisrupted extends Outcome {
    def returnCode: ReturnCode
    def keyValues: Map[String, String]
  }

  object Undisrupted {
    def apply(success: Boolean, returnCode: ReturnCode): Undisrupted =
      apply(success, returnCode, Map.empty)

    def apply(success: Boolean, returnCode: ReturnCode, keyValues: Map[String, String]): Undisrupted =
      if (success)
        Succeeded(returnCode, keyValues)
      else
        Failed(returnCode, keyValues)

    private[Outcome] sealed trait Companion[A <: Undisrupted] {
      protected def newInstance(returnCode: ReturnCode, keyValues: Map[String, String]): A

      // re-use memory for usual values.
      private lazy val usualValues: Vector[A] = (0 to 255).map(i => newInstance(ReturnCode(i), Map.empty)).toVector

      def apply(returnCode: ReturnCode, keyValues: Map[String, String] = Map.empty): A =
        if (keyValues.isEmpty && usualValues.indices.contains(returnCode.number))
          usualValues(returnCode.number)
        else
          newInstance(returnCode, keyValues)
    }
  }

  final case class Succeeded(returnCode: ReturnCode, keyValues: Map[String, String] = Map.empty) extends Undisrupted {
    def isSucceeded = true
  }
  object Succeeded extends Undisrupted.Companion[Succeeded]
  {
    def apply(keyValues: Map[String, String]): Succeeded = apply(ReturnCode.Success, keyValues)

    protected def newInstance(returnCode: ReturnCode, keyValues: Map[String, String]): Succeeded =
      new Succeeded(returnCode, keyValues)

    implicit val jsonEncoder: Encoder.AsObject[Succeeded] =
      o => JsonObject.fromIterable(
        ("returnCode" -> o.returnCode.asJson) ::
          o.keyValues.nonEmpty.thenList("keyValues" -> o.keyValues.asJson))

    implicit val jsonDecoder: Decoder[Succeeded] =
      c => for {
        returnCode <- c.get[ReturnCode]("returnCode")
        keyValues <- c.get[Option[Map[String, String]]]("keyValues") map (_ getOrElse Map.empty)
      } yield Succeeded(returnCode, keyValues)
  }

  final case class Failed(errorMessage: Option[String], returnCode: ReturnCode, keyValues: Map[String, String])
  extends Undisrupted with NotSucceeded
  {
    def isSucceeded = false
  }
  object Failed extends Undisrupted.Companion[Failed]
  {
    val DefaultErrorMessage = "Failed"

    def apply(errorMessage: Option[String], returnCode: ReturnCode): Failed =
      Failed(errorMessage, returnCode, Map.empty)

    protected def newInstance(returnCode: ReturnCode, keyValues: Map[String, String]): Failed =
      Failed(errorMessage = None, returnCode, keyValues)

    implicit val jsonEncoder: Encoder.AsObject[Failed] =
      o => JsonObject.fromIterable(
        ("message" -> o.errorMessage.asJson) ::
        ("returnCode" -> o.returnCode.asJson) ::
        o.keyValues.nonEmpty.thenList("keyValues" -> o.keyValues.asJson))

    implicit val jsonDecoder: Decoder[Failed] =
      c => for {
        errorMessage <- c.get[Option[String]]("message")
        returnCode <- c.get[ReturnCode]("returnCode")
        keyValues <- c.get[Option[Map[String, String]]]("keyValues") map (_ getOrElse Map.empty)
      } yield Failed(errorMessage, returnCode, keyValues)
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
    Subtype(deriveCodec[Disrupted]))
}
