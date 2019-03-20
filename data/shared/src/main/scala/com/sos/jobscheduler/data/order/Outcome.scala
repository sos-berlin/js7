package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.job.ReturnCode
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
    def apply(success: Boolean, returnCode: ReturnCode, keyValues: Map[String, String] = Map.empty): Undisrupted =
      if (success)
        Succeeded(returnCode, keyValues)
      else
        Failed(returnCode, keyValues)

    def unapply(undisrupted: Undisrupted): Some[(Boolean, ReturnCode, Map[String, String])] =
      Some(undisrupted match {
        case Succeeded(returnCode, keyValues) => (true, returnCode, keyValues)
        case Failed(returnCode, keyValues) => (false, returnCode, keyValues)
      })

    private[Outcome] sealed trait Companion[A <: Undisrupted] {
      def newInstance(returnCode: ReturnCode, keyValues: Map[String, String]): A

      // re-use memory for usual values.
      private val usualValues: Vector[A] = (0 to 255).map(i => newInstance(ReturnCode(i), Map.empty)).toVector

      def apply(returnCode: ReturnCode, keyValues: Map[String, String] = Map.empty): A =
        if (keyValues.isEmpty && usualValues.indices.contains(returnCode.number))
          usualValues(returnCode.number)
        else
          newInstance(returnCode, keyValues)
    }
  }

  final case class Succeeded(returnCode: ReturnCode, keyValues: Map[String, String] = Map.empty) extends Undisrupted {
    def isSucceeded = returnCode.isSuccess
  }
  object Succeeded extends Undisrupted.Companion[Succeeded]
  {
    def apply(keyValues: Map[String, String]): Succeeded = apply(ReturnCode.Success, keyValues)

    def newInstance(returnCode: ReturnCode, keyValues: Map[String, String]): Succeeded =
      new Succeeded(returnCode, keyValues)

    implicit val jsonEncoder: ObjectEncoder[Succeeded] =
      o => JsonObject.fromIterable(
        ("returnCode" -> o.returnCode.asJson) ::
          o.keyValues.nonEmpty.thenList("keyValues" -> o.keyValues.asJson))

    implicit val jsonDecoder: Decoder[Succeeded] =
      c => for {
        returnCode <- c.get[ReturnCode]("returnCode")
        keyValues <- c.get[Option[Map[String, String]]]("keyValues") map (_ getOrElse Map.empty)
      } yield Succeeded(returnCode, keyValues)
  }

  final case class Failed(returnCode: ReturnCode, keyValues: Map[String, String] = Map.empty)
  extends Undisrupted with NotSucceeded
  {
    def isSucceeded = false
  }
  object Failed extends Undisrupted.Companion[Failed]
  {
    def newInstance(returnCode: ReturnCode, keyValues: Map[String, String]): Failed =
      new Failed(returnCode, keyValues)

    implicit val jsonEncoder: ObjectEncoder[Failed] =
      o => JsonObject.fromIterable(
        ("returnCode" -> o.returnCode.asJson) ::
          o.keyValues.nonEmpty.thenList("keyValues" -> o.keyValues.asJson))

    implicit val jsonDecoder: Decoder[Failed] =
      c => for {
        returnCode <- c.get[ReturnCode]("returnCode")
        keyValues <- c.get[Option[Map[String, String]]]("keyValues") map (_ getOrElse Map.empty)
      } yield Failed(returnCode, keyValues)
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
      Subtype[Failed],
      Subtype(deriveCodec[Disrupted]))
  }

  implicit val jsonCodec = TypedJsonCodec[Outcome](
    Subtype[Succeeded],
    Subtype[Failed],
    Subtype(deriveCodec[Disrupted]))
}
