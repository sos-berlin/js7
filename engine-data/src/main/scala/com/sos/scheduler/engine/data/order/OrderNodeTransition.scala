package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.job.ReturnCode
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
sealed trait OrderNodeTransition

object OrderNodeTransition {

  def ofCppInternalValue(internalValue: Long) = internalValue match {
    case Long.MaxValue ⇒
      Keep
    case i ⇒
      assert(i.toInt == i, s"OrderNodeTransition($i)")
      Proceeding(ReturnCode(i.toInt))
  }

  /**
   * Order proceeds to another jobchain node.
   */
  trait Proceeding extends OrderNodeTransition {
    def returnCode: ReturnCode
  }

  object Proceeding {
    def apply(returnCode: ReturnCode) = returnCode match {
      case ReturnCode.Success ⇒ Success
      case rc ⇒ Error(rc)
    }

    def unapply(o: Proceeding) = Some(o.returnCode)
  }

  /**
   * Order proceeds to another jobchain node, used by attribute "next_state".
   */
  case object Success extends Proceeding {
    def returnCode = ReturnCode.Success
  }

  /**
   * Order proceeds to another jobchain node, used by attribute "error_state".
   */
  final case class Error(returnCode: ReturnCode) extends Proceeding

  object Error {
    def Standard = Error(ReturnCode.StandardFailure)
  }

  /**
   * Order step could not been completed and order stays in same jobchain node.
   */
  case object Keep extends OrderNodeTransition

  implicit val OrderNodeTransitionJsonFormat = TypedJsonFormat[OrderNodeTransition](
    Subtype(jsonFormat0(() ⇒ Keep)),
    Subtype(jsonFormat1(Error.apply)),
    Subtype(jsonFormat0(() ⇒ Success)))
}
