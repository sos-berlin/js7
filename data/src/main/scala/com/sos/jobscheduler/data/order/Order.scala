package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
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
  agentPathOption: Option[AgentPath] = None)
{
  def jobnetPath: JobnetPath =
    nodeKey.jobnetPath

  def nodeId: NodeId =
    nodeKey.nodeId

  def update(event: OrderEvent): Order[State] =
    event match {
      case OrderAdded(nodeKey_, state_, variables_, outcome_) ⇒
        copy(nodeKey = nodeKey_, state = state_, variables = variables_, outcome = outcome_)

      case OrderAttached(nodeKey_, state_, variables_, outcome_) ⇒  // Doppelt mit OrderAdded ???
        copy(nodeKey = nodeKey_, state = state_, variables = variables_, outcome = outcome_)

      case OrderMovedToAgent(agentPath) ⇒ copy(
        agentPathOption = Some(agentPath))

      case OrderMovedToMaster ⇒ copy(
        state = Detached,
        agentPathOption = None)

      case OrderStepStarted ⇒ copy(
        state = InProcess)

      case OrderStepSucceeded(diff, returnValue) ⇒ copy(
        state = Waiting,
        variables = diff.applyTo(variables),
        outcome = Good(returnValue))

      case OrderStepFailed(error) ⇒ copy(
        state = Waiting,
        outcome = Bad(error))

      case OrderNodeChanged(nodeId) ⇒ copy(
        state = Waiting,
        nodeKey = nodeKey.copy(nodeId = nodeId))

      case OrderReady ⇒ copy(
        state = Ready)

      case OrderDetached ⇒ copy(
        state = Detached)

      case OrderFinished ⇒ copy(
        state = Finished)
    }

  def castAfterEvent(event: OrderStepStarted.type): Order[Order.InProcess.type] =
    castState[Order.InProcess.type]

  def castState[T <: State: ClassTag]: Order[T] = {
    if (!implicitClass[T].isAssignableFrom(state.getClass))
      throw new ClassCastException(s"Order '$id': Order[${state.getClass.getSimpleName stripSuffix "$"}] cannot be cast to Order[${implicitClass[T].getSimpleName stripSuffix "$"}]")
    this.asInstanceOf[Order[T]]
  }
}

object Order {
  val InitialOutcome = Good(true)

  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  sealed trait Outcome

  object Outcome {
    private implicit val succeededJsonFormat: RootJsonFormat[Good] = jsonFormat1(Good)
    private implicit val failedJsonFormat: RootJsonFormat[Bad] = jsonFormat1(Bad)

    implicit val outcomeJsonFormat =
      new RootJsonFormat[Outcome] {
        def write(o: Outcome) =
          o match {
            case o: Good ⇒ o.toJson
            case o: Bad ⇒ o.toJson
          }

        def read(json: JsValue) = {
          if (json.asJsObject.fields contains "error")
            json.convertTo[Bad]
          else
            json.convertTo[Good]
        }
      }
  }

  final case class Good(returnValue: Boolean) extends Outcome
  final case class Bad(error: String) extends Outcome

  implicit val IdleJsonFormat: TypedJsonFormat[Idle] = TypedJsonFormat(
    Subtype(jsonFormat1(Scheduled)),
    Subtype(jsonFormat0(() ⇒ Detached)),
    Subtype(jsonFormat0(() ⇒ Waiting)))

  implicit val StateJsonFormat: TypedJsonFormat[State] = TypedJsonFormat(
    Subtype[Idle],
    Subtype(jsonFormat1(Scheduled)),
    Subtype(jsonFormat0(() ⇒ StartNow)),
    Subtype(jsonFormat0(() ⇒ InProcess)),
    Subtype(jsonFormat0(() ⇒ Ready)),
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
  final case object StartNow extends NotStarted  // == Scheduled(None) ???
  //final case class Postponed(until: Instant) extends Started with Idle
  //case object Suspended extends Started with Idle
  case object Waiting extends Started with Idle
  case object Ready extends Started  // Retrievable ???
  case object InProcess extends Started
  //case object AfterStep extends Started
  //case object Done extends State
  case object Detached extends Started with Idle
  case object Finished extends State
}
