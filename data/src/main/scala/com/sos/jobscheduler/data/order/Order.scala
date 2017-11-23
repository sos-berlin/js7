package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.order.OrderEvent._
import java.time.Instant
import scala.reflect.ClassTag
import spray.json.DefaultJsonProtocol._
import spray.json._

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
        nodeKey = NodeKey(jobnetPath, toNodeId))

      case OrderDetachable ⇒ copy(
        state = Detachable)

      case OrderDetached ⇒ copy(
        state = Detached)

      case OrderFinished ⇒ copy(
        state = Finished)
    }

  def payload = Payload(variables, outcome)

  def castAfterEvent(event: OrderProcessingStarted.type): Order[Order.InProcess.type] =
    castState[Order.InProcess.type]

  def castAfterEvent(event: OrderProcessed.type): Order[Order.Processed.type] =
    castState[Order.Processed.type]

  def castState[T <: State: ClassTag]: Order[T] = {
    if (!implicitClass[T].isAssignableFrom(state.getClass))
      throw new ClassCastException(s"Order '$id': Order[${state.getClass.getSimpleName stripSuffix "$"}] cannot be cast to Order[${implicitClass[T].getSimpleName stripSuffix "$"}]")
    this.asInstanceOf[Order[T]]
  }
}

object Order {
  private implicit def InstantJsonFormat = JavaTimeJsonFormats.NumericInstantJsonFormat  // Override default

  val InitialOutcome = Good(true)

  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  final case class Payload(variables: Map[String, String], outcome: Order.Outcome = InitialOutcome) {
    override def toString = s"Payload($outcome ${(for (k ← variables.keys.toVector.sorted) yield s"$k=${variables(k)}") mkString ", "}".trim + ")"
  }

  object Good {
    private val False = new Good(false)
    private val True = new Good(true)

    def apply(returnValue: Boolean) = if (returnValue) True else False
  }

  final case class Bad(reason: Bad.Reason) extends Outcome {
    def isSuccess = false
  }

  object Bad {
    def apply(message: String): Bad =
      Bad(Other(message))

    sealed trait Reason {
      def message: String
    }
    final case object AgentAborted extends Reason {
      def message = "Agent aborted while order was InProcess"
    }
    final case class Other(message: String) extends Reason

    object Reason {
      implicit val jsonFormat = TypedJsonFormat[Reason](
        Subtype(jsonFormat0(() ⇒ AgentAborted)),
        Subtype(jsonFormat1(Other)))
    }
  }

  sealed trait Outcome {
    def isSuccess: Boolean
  }

  object Outcome {
    implicit val outcomeJsonFormat = TypedJsonFormat[Outcome](
      Subtype(jsonFormat1(Good.apply)),
      Subtype(jsonFormat1((o: Bad.Reason) ⇒ Bad(o))))
  }

  final case class Good private(returnValue: Boolean) extends Outcome {
    def isSuccess = returnValue
  }

  implicit val IdleJsonFormat: TypedJsonFormat[Idle] = TypedJsonFormat(
    Subtype(jsonFormat1(Scheduled)),
    Subtype(jsonFormat0(() ⇒ StartNow)),
    Subtype(jsonFormat0(() ⇒ Detached)),
    Subtype(jsonFormat0(() ⇒ Ready)))

  implicit val StateJsonFormat: TypedJsonFormat[State] = TypedJsonFormat(
    Subtype[Idle],
    Subtype(jsonFormat1(Scheduled)),
    Subtype(jsonFormat0(() ⇒ StartNow)),
    Subtype(jsonFormat0(() ⇒ InProcess)),
    Subtype(jsonFormat0(() ⇒ Processed)),
    Subtype(jsonFormat0(() ⇒ Detachable)),
    Subtype(jsonFormat0(() ⇒ Finished)))

  private implicit val rootJsonFormat: RootJsonFormat[Order[State]] =
    jsonFormat6((a: OrderId, b: NodeKey, c: State, d: Map[String, String], e: Order.Outcome, f: Option[AgentPath]) ⇒
      Order(a, b, c, d, e, f))

  implicit def jsonFormat[S <: Order.State]: RootJsonFormat[Order[S]] =
    rootJsonFormat.asInstanceOf[RootJsonFormat[Order[S]]]

  sealed trait State
  sealed trait Idle extends State
  sealed trait NotStarted extends Idle
  sealed trait Started extends State
  final case class Scheduled(at: Instant) extends NotStarted
  final case object StartNow extends NotStarted
  case object Ready extends Started with Idle
  case object InProcess extends Started
  case object Processed extends Started
  case object Detachable extends Started
  case object Detached extends Started with Idle
  case object Finished extends State
}
