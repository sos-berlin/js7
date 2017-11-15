package com.sos.jobscheduler.master.gui.data

import com.sos.jobscheduler.master.gui.data.Order._
import com.sos.jobscheduler.master.gui.data.OrderEvent._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/**
  * @author Joacim Zschimmer
  */
final case class Order[+S <: Order.State](
  id: OrderId,
  nodeKey: NodeKey,
  state: S,
  variables: Map[String, String] = Map(),
  outcome: Outcome = InitialOutcome,
  agentPath: Option[AgentPath] = None)
{
  def jobnetPath: JobnetPath =
    nodeKey.jobnetPath

  def nodeId: NodeId =
    nodeKey.nodeId

  def update(event: OrderEvent.OrderCoreEvent): Order[State] =
    event match {
      case OrderAdded(nodeKey_, state_, variables_, outcome_) ⇒
        copy(nodeKey = nodeKey_, state = state_, variables = variables_, outcome = outcome_)

      case OrderAttached(nodeKey_, state_, variables_, outcome_) ⇒
        copy(nodeKey = nodeKey_, state = state_, variables = variables_, outcome = outcome_)

      case OrderMovedToAgent(o) ⇒ copy(
        agentPath = Some(o))

      case OrderMovedToMaster ⇒ copy(
        state = Detached,
        agentPath = None)

      case OrderStepStarted ⇒ copy(
        state = InProcess)

      case OrderStepSucceeded(diff, returnValue, nextNodeId) ⇒ copy(
        state = Waiting,
        variables = diff.applyTo(variables),
        outcome = Good(returnValue),
        nodeKey = NodeKey(jobnetPath,  nextNodeId))

      case OrderStepFailed(reason, nextNodeId) ⇒ copy(
        state = Waiting,
        outcome = Bad(reason.message),
        nodeKey = NodeKey(jobnetPath,  nextNodeId))

      case OrderReady ⇒ copy(
        state = Ready)

      case OrderDetached ⇒ copy(
        state = Detached)

      case OrderFinished ⇒ copy(
        state = Finished)
    }
}

object Order {
  val InitialOutcome = Good(true)

  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  sealed trait Outcome
  final case class Good private(returnValue: Boolean) extends Outcome
  object Good {
    private val False = new Good(false)
    private val True = new Good(true)

    def apply(returnValue: Boolean) = if (returnValue) True else False
  }

  final case class Bad(error: String) extends Outcome

  sealed trait State
  sealed trait Idle extends State
  sealed trait NotStarted extends Idle
  sealed trait Started extends State
  final case class Scheduled(at: Timestamp) extends NotStarted
  final case object StartNow extends NotStarted
  case object Waiting extends Started with Idle
  case object Ready extends Started
  case object InProcess extends Started
  case object Detached extends Started with Idle
  case object Finished extends State

  private implicit val scheduledJsonDecoder = deriveDecoder[Scheduled]
  private implicit val startNowJsonDecoder = Decoder.const(StartNow)
  private implicit val waitingJsonDecoder = Decoder.const(Waiting)
  private implicit val detachedJsonDecoder = Decoder.const(Detached)

  object Idle {
    implicit  val jsonDecoder: Decoder[Idle] = {
      cursor ⇒
        for {
          typ ← cursor.downField("TYPE").as[String]
          state ← typ match {
            case "Scheduled" ⇒ scheduledJsonDecoder(cursor)
            case "StartNow" ⇒ startNowJsonDecoder(cursor)
            case "Waiting" ⇒ waitingJsonDecoder(cursor)
            case "Detached" ⇒ detachedJsonDecoder(cursor)
            case _ ⇒ sys.error(s"Unknown TYPE=$typ")  // throw ???
          }
        } yield state
    }
  }

  object State {
    implicit val jsonDecoder: Decoder[State] =
      cursor ⇒
        for {
          typ ← cursor.downField("TYPE").as[String]
          state ← typ match {
            case "Scheduled" | "StartNow" | "Waiting" | "Detached" ⇒ Idle.jsonDecoder(cursor)
            case "Ready" ⇒ Decoder.const(Ready)(cursor)
            case "InProcess" ⇒ Decoder.const(InProcess)(cursor)
            case "Finished" ⇒ Decoder.const(Finished)(cursor)
          }
        } yield state
  }

  implicit val outcomeDecoder: Decoder[Outcome] =
    cursor ⇒
      cursor.downField("error").success match {
        case Some(errorCursor) ⇒ for (error ← errorCursor.as[String]) yield Bad(error)
        case None ⇒ for (returnValue ← cursor.downField("returnValue").as[Boolean]) yield Good(returnValue)
      }

  implicit val jsonDecoder: Decoder[Order[State]] = deriveDecoder[Order[State]]
}
