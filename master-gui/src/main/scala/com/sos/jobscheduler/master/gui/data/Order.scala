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

      case OrderProcessingStarted ⇒ copy(
        state = InProcess)

      case OrderProcessed(diff, outcome_) ⇒ copy(
        state = Processed,
        variables = diff.applyTo(variables),
        outcome = outcome_)

      case OrderTransitioned(toNodeId) ⇒ copy(
        state = Ready,
        nodeKey = NodeKey(jobnetPath,  toNodeId))

      case OrderDetachable ⇒ copy(
        state = Detachable)

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
    implicit val jsonDecoder: Decoder[Good] = deriveDecoder[Good]
    private val False = new Good(false)
    private val True = new Good(true)

    def apply(returnValue: Boolean) = if (returnValue) True else False
  }

  final case class Bad(error: String) extends Outcome
  object Bad {
    implicit val jsonDecoder: Decoder[Bad] = deriveDecoder[Bad]
  }

  sealed trait State
  sealed trait Idle extends State
  sealed trait NotStarted extends Idle
  sealed trait Started extends State
  final case class Scheduled(at: Timestamp) extends NotStarted
  final case object StartNow extends NotStarted
  case object Ready extends Started with Idle
  case object Detachable extends Started
  case object InProcess extends Started
  case object Processed extends Started
  case object Detached extends Started with Idle
  case object Finished extends State

  private implicit val scheduledJsonDecoder = deriveDecoder[Scheduled]
  private implicit val startNowJsonDecoder = Decoder.const(StartNow)
  private implicit val waitingJsonDecoder = Decoder.const(Ready)
  private implicit val detachedJsonDecoder = Decoder.const(Detached)


  object Idle {
    implicit  val jsonDecoder: Decoder[Idle] = {
      cursor ⇒
        for {
          typ ← cursor.downField("TYPE").as[String]
          state ← typ match {
            case "Scheduled" ⇒ scheduledJsonDecoder(cursor)
            case "StartNow" ⇒ startNowJsonDecoder(cursor)
            case "Ready" ⇒ waitingJsonDecoder(cursor)
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
            case "Scheduled" | "StartNow" | "Ready" | "Detached" ⇒ Idle.jsonDecoder(cursor)
            case "Detachable" ⇒ Decoder.const(Detachable)(cursor)
            case "InProcess" ⇒ Decoder.const(InProcess)(cursor)
            case "Processed" ⇒ Decoder.const(Processed)(cursor)
            case "Finished" ⇒ Decoder.const(Finished)(cursor)
          }
        } yield state
  }

  implicit val outcomeDecoder: Decoder[Outcome] =
    cursor ⇒
      for {
        typ ← cursor.downField("TYPE").as[String]
        outcome ← typ match {
          case "Good" ⇒ implicitly[Decoder[Good]].apply(cursor)
          case "Bad" ⇒ implicitly[Decoder[Bad]].apply(cursor)
        }
      } yield outcome

  implicit val jsonDecoder: Decoder[Order[State]] = deriveDecoder[Order[State]]
}
