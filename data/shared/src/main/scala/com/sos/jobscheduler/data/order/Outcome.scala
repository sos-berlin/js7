package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.job.ReturnCode
import io.circe.generic.JsonCodec

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
  }

  sealed trait NotSucceeded extends Outcome
  object NotSucceeded {
    implicit val jsonCodec = TypedJsonCodec[NotSucceeded](
      Subtype[Failed],
      Subtype[Disrupted])
  }

  object Undisrupted {
    def apply(returnCode: ReturnCode, success: Boolean): Undisrupted =
      if (success)
        Succeeded(returnCode)
      else
        Failed(returnCode)

    private[Outcome] sealed trait Companion[A <: Undisrupted] {
      def newInstance(returnCode: ReturnCode): A

      // re-use memory for usual values.
      private val usualValues: Vector[A] = (0 to 255).map(i ⇒ newInstance(ReturnCode(i))).toVector

      def apply(returnCode: ReturnCode): A =
        if (usualValues.indices contains returnCode.number)
          usualValues(returnCode.number)
        else
          newInstance(returnCode)
    }
  }

  @JsonCodec
  final case class Succeeded(returnCode: ReturnCode) extends Undisrupted {
    def isSucceeded = returnCode.isSuccess
  }
  object Succeeded extends Undisrupted.Companion[Succeeded] {
    def newInstance(returnCode: ReturnCode): Succeeded = new Succeeded(returnCode)
  }

  @JsonCodec
  final case class Failed(returnCode: ReturnCode) extends Undisrupted with NotSucceeded {
    def isSucceeded = false
  }
  object Failed extends Undisrupted.Companion[Failed] {
    def newInstance(returnCode: ReturnCode): Failed = new Failed(returnCode)
  }

  /** No response from job - some other error has occurred. */
  @JsonCodec
  final case class Disrupted(reason: Disrupted.Reason) extends Outcome with NotSucceeded {
    def isSucceeded = false
  }
  object Disrupted {
    def apply(message: String): Disrupted =
      Disrupted(Other(message))

    sealed trait Reason {
      def message: String
    }
    final case object JobSchedulerRestarted extends Reason {
      def message = "JobScheduler stopped while order was in-process"
    }

    @JsonCodec
    final case class Other(message: String) extends Reason

    object Reason {
      implicit val jsonCodec = TypedJsonCodec[Reason](
        Subtype(JobSchedulerRestarted),
        Subtype[Other])
    }
  }

  implicit val jsonCodec = TypedJsonCodec[Outcome](
    Subtype[Succeeded],
    Subtype[Failed],
    Subtype[Disrupted])
}
