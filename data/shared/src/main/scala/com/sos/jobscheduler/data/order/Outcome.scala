package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
sealed trait Outcome {
  def isSuccess: Boolean
}

object Outcome {
  val Default = Good(true)

  @JsonCodec
  final case class Good private(returnValue: Boolean) extends Outcome {
    def isSuccess = returnValue
  }
  object Good {
    private val False = new Good(false)
    private val True = new Good(true)

    def apply(returnValue: Boolean) = if (returnValue) True else False
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

