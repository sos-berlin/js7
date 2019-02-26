package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.job.ReturnCode

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

  object Undisrupted {
    def apply(returnCode: ReturnCode, success: Boolean): Undisrupted =
      if (success)
        Succeeded(returnCode)
      else
        Failed(returnCode)

    def unapply(undisrupted: Undisrupted): Some[(ReturnCode, Boolean)] =
      Some(undisrupted match {
        case Succeeded(returnCode) => returnCode -> true
        case Failed(returnCode) => returnCode -> false
      })

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

  final case class Succeeded(returnCode: ReturnCode) extends Undisrupted {
    def isSucceeded = returnCode.isSuccess
  }
  object Succeeded extends Undisrupted.Companion[Succeeded] {
    def newInstance(returnCode: ReturnCode): Succeeded = new Succeeded(returnCode)
  }

  final case class Failed(returnCode: ReturnCode) extends Undisrupted with NotSucceeded {
    def isSucceeded = false
  }
  object Failed extends Undisrupted.Companion[Failed] {
    def newInstance(returnCode: ReturnCode): Failed = new Failed(returnCode)
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
      val problem = Problem.pure("JobScheduler stopped while order was in-process")
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
      Subtype(deriveCodec[Failed]),
      Subtype(deriveCodec[Disrupted]))
  }

  implicit val jsonCodec = TypedJsonCodec[Outcome](
    Subtype(deriveCodec[Succeeded]),
    Subtype(deriveCodec[Failed]),
    Subtype(deriveCodec[Disrupted]))
}
