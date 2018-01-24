package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.job.ReturnCode
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait Outcome {
  /** Default semantic of error. */
  def isError: Boolean = !isSuccess

  /** Default semantics of success. */
  def isSuccess: Boolean
}

object Outcome {
  val Default = Good(ReturnCode.Success)

  @JsonCodec
  final case class Good private(returnCode: ReturnCode) extends Outcome {
    def isSuccess = returnCode.isSuccess
  }
  object Good {
    private val constants: Vector[Good] = (0 to 255).map(i ⇒ new Good(ReturnCode(i))).toVector

    def apply(returnCode: ReturnCode): Good =
      if (constants.indices contains returnCode.number)
        constants(returnCode.number)  // Reduce memory
      else
        new Good(returnCode)
  }

  @JsonCodec
  final case class Bad(reason: Bad.Reason) extends Outcome {
    def isSuccess = false
  }
  object Bad {
    def apply(message: String): Bad =
      Bad(Other(message))

    sealed trait Reason {
      def message: String
    }
    final case object AgentRestarted extends Reason {
      def message = "Agent has been restarted while order was processed"
    }

    @JsonCodec
    final case class Other(message: String) extends Reason

    object Reason {
      implicit val jsonCodec = TypedJsonCodec[Reason](
        Subtype(AgentRestarted),
        Subtype[Other])
    }
  }

  implicit val jsonCodec = TypedJsonCodec[Outcome](
    Subtype[Good],
    Subtype[Bad])
}
