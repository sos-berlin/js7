package com.sos.jobscheduler.master.gui.data

import com.sos.jobscheduler.master.gui.common.system.StdoutStderr._
import com.sos.jobscheduler.master.gui.data.event.Event
import io.circe.Decoder
import io.circe.generic.auto._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderEvent extends Event {
  type Key = OrderId
}

object OrderEvent {
  sealed trait OrderCoreEvent extends OrderEvent

  final case class OrderAdded(
    nodeKey: NodeKey,
    state: Order.Idle,
    variables: Map[String, String],
    outcome: Order.Outcome)
  extends OrderCoreEvent

  final case class OrderAttached(
    nodeKey: NodeKey,
    state: Order.Idle,
    variables: Map[String, String],
    outcome: Order.Outcome)
  extends OrderCoreEvent

  final case class OrderMovedToAgent(agentPath: AgentPath)
  extends OrderCoreEvent

  final case object OrderMovedToMaster
  extends OrderCoreEvent

  case object OrderStepStarted extends OrderCoreEvent


  sealed trait OrderStdWritten extends OrderEvent {
    def stdoutStderrType: StdoutStderrType
  }

  object OrderStdWritten {
    def apply(t: StdoutStderrType): String ⇒ OrderStdWritten =
      t match {
        case Stdout ⇒ OrderStdoutWritten
        case Stderr ⇒ OrderStderrWritten
      }

    def unapply(o: OrderStdWritten) = o match {
      case OrderStdoutWritten(chunk) ⇒ Some((Stdout, chunk))
      case OrderStderrWritten(chunk) ⇒ Some((Stderr, chunk))
    }
  }

  final case class OrderStdoutWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderrType = Stdout
  }

  final case class OrderStderrWritten(chunk: String) extends OrderStdWritten {
    def stdoutStderrType = Stderr
  }


  sealed trait OrderStepEnded extends OrderCoreEvent {
    def nextNodeId: NodeId
  }

  final case class OrderStepSucceeded(
    variablesDiff: MapDiff[String, String],
    returnValue: Boolean,
    nextNodeId: NodeId)
  extends OrderStepEnded

  final case class OrderStepFailed(reason: OrderStepFailed.Reason, nextNodeId: NodeId)
  extends OrderStepEnded

  object OrderStepFailed {
    sealed trait Reason {
      def message: String
    }
    final case object AgentAborted extends Reason {
      def message = "Agent aborted while order was InProcess"
    }
    final case class Other(message: String) extends Reason

    object Reason {
      implicit val jsonDecoder: Decoder[Reason] =
        cursor ⇒
          for {
            typ ← cursor.downField("TYPE").as[String]
            reason ← typ match {
              case "AgentAborted" ⇒ Decoder.const(AgentAborted)(cursor)
              case "Other" ⇒ for (message ← cursor.downField("message").as[String]) yield Other(message)
            }
          } yield reason
    }
  }

  /**
    * Agent has processed all steps and the Order should be fetched by the Master.
    */
  case object OrderDetachable extends OrderCoreEvent

  /**
    * Order has been removed from the Agent and is held by the Master.
    */
  case object OrderDetached extends OrderCoreEvent

  case object OrderFinished extends OrderCoreEvent

  implicit val jsonDecoder: Decoder[OrderEvent] =
    cursor ⇒
      for {
        typ ← cursor.downField("TYPE").as[String]
        event ← (typ match {
          case "OrderAdded" ⇒ implicitly[Decoder[OrderAdded]]
          case "OrderAttached" ⇒ implicitly[Decoder[OrderAttached]]
          case "OrderMovedToAgent" ⇒ implicitly[Decoder[OrderMovedToAgent]]
          case "OrderMovedToMaster" ⇒ Decoder.const(OrderMovedToMaster)
          case "OrderStepStarted" ⇒ Decoder.const(OrderStepStarted)
          case "OrderStderrWritten" ⇒ implicitly[Decoder[OrderStderrWritten]]
          case "OrderStdoutWritten" ⇒ implicitly[Decoder[OrderStdoutWritten]]
          case "OrderStepFailed" ⇒ implicitly[Decoder[OrderStepFailed]]
          case "OrderStepSucceeded" ⇒ implicitly[Decoder[OrderStepSucceeded]]
          case "OrderDetachable" ⇒ Decoder.const(OrderDetachable)
          case "OrderDetached" ⇒ Decoder.const(OrderDetached)
          case "OrderFinished" ⇒ Decoder.const(OrderFinished)
        }) apply cursor
      } yield event
}
