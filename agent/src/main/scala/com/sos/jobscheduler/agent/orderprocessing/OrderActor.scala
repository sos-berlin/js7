package com.sos.jobscheduler.agent.orderprocessing

import akka.actor.{ActorRef, Status, Terminated}
import com.sos.jobscheduler.agent.orderprocessing.OrderActor._
import com.sos.jobscheduler.agent.orderprocessing.job.JobRunner
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import com.sos.jobscheduler.data.engine2.order.JobNet.JobNode
import com.sos.jobscheduler.data.engine2.order.OrderEvent._
import com.sos.jobscheduler.data.engine2.order.{Order, OrderEvent}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.shared.event.journal.KeyedJournalingActor

/**
  * @author Joacim Zschimmer
  */
private final class OrderActor(orderId: OrderId, protected val journalActor: ActorRef)
extends KeyedJournalingActor[OrderEvent] {

  private val logger = Logger.withPrefix[OrderActor](orderId.string)
  private var order: Order[Order.State] = null

  protected def key = orderId
  protected def snapshot = order

  protected def recoverFromSnapshot(snapshot: Any) = {
    assert(order == null)
    order = cast[Order[Order.State]](snapshot)
  }

  protected def recoverFromEvent(event: OrderEvent) = updateOnly(event)

  def receive = {
    case command: Command ⇒ command match {
      case Command.Attach(Order(`orderId`, nodeKey, state: Order.Idle, variables, outcome, _: Option[AgentPath])) ⇒
        context.become(waiting)
        persist(OrderAttached(nodeKey, state, variables, outcome)) { event ⇒
          update(event)
          sender() ! Completed
        }

      case _ ⇒
        executeOtherCommand(command)
    }

    case Input.FinishRecovery ⇒
      assert(order != null, "No Order")
      context.parent ! Output.RecoveryFinished(order)
      order.state match {
        case Order.Waiting ⇒
          context.become(waiting)

        case Order.InProcess ⇒
          context.become(waiting)
          persist(OrderStepFailed(s"Agent aborted while order was InProcess"))(update)
          persist(OrderNodeChanged(order.nodeId))(update)  // Nothing changes, but triggers AgentOrderKeeper. Vielleicht OrderNodeChange mit OrderStepEnded verschmelzen ???

        case Order.StartNow | _: Order.Scheduled | Order.Ready | Order.Detached | Order.Finished ⇒
          context.become(waiting)

        case _ ⇒
      }
  }

  private val waiting: Receive = {
    case Command.Detach ⇒
      persist(OrderDetached) { event ⇒
        update(event)
        sender() ! Completed
      }

    case command: Command ⇒
      executeOtherCommand(command)

    case input: Input ⇒
      executeInput(input)
  }

  private def executeOtherCommand(command: Command): Unit = command match {
    case Command.GetSnapshot ⇒
      sender() ! order

    case _ ⇒
      val msg = s"Improper command $command while in state ${order.state}"
      logger.error(msg)
      sender() ! Status.Failure(new IllegalStateException(msg))
  }

  private def executeInput(input: Input) = input match {
    case Input.StartStep(node, jobActor) ⇒
      context.become(processing(node, jobActor))
      context.watch(jobActor)
      persist(OrderStepStarted) { event ⇒
        update(event)
        jobActor ! JobRunner.Command.ProcessOrder(order.castAfterEvent(event))
      }

    case Input.SetReady ⇒
      persist(OrderReady)(update)
  }

  private def processing(node: JobNode, jobActor: ActorRef): Receive = {
    case JobRunner.Response.OrderProcessed(`orderId`, event: OrderStepEnded) if node != null ⇒
      endOrderStep(event, node)

    case Terminated(`jobActor`) ⇒
      endOrderStep(OrderStepFailed(s"JobActor ${node.jobPath.string} terminated unexpectedly"), node)

    case command: Command ⇒
      executeOtherCommand(command)
  }

  private def endOrderStep(event: OrderStepEnded, node: JobNode): Unit = {
    context.become(waiting)
    persist(event)(update)
    val nextNodeId = event match {
      case event: OrderStepSucceeded ⇒ if (event.returnValue) node.onSuccess else node.onFailure
      case _: OrderStepFailed ⇒ node.onFailure
    }
    persist(OrderNodeChanged(nextNodeId))(update)
  }

  private def update(event: OrderEvent) = {
    updateOnly(event)
    context.parent ! Output.OrderChanged(order, event)
  }

  private def updateOnly(event: OrderEvent) = {
    order = event match {
      case OrderAttached(nodeKey_, state_, variables_, outcome_) ⇒
        Order(orderId, nodeKey = nodeKey_, state = state_, outcome = outcome_, variables = variables_)
        // Order.state = Attached / MovedToAgent ???

      case OrderDetached ⇒
        logger.trace("Stopping after OrderDetached")
        context.stop(self)
        order

      case _ if order != null ⇒
        order.update(event)

      case _ ⇒
        sys.error(s"Not an initial OrderEvent for '$orderId': $event")
    }
  }

  override def unhandled(msg: Any) = {
    msg match {
      case msg @ (_: Command | _: Input) ⇒ logger.warn(s"Unhandled message $msg in state ${order.state}")
      case _ ⇒
    }
    super.unhandled(msg)
  }

  override def toString = s"OrderActor(${orderId.string})"
}

object OrderActor {

  sealed trait Command
  object Command {
    final case class  Attach(order: Order[Order.Idle]) extends Command
    final case object Detach extends Command
    final case object GetSnapshot extends Command
  }

  sealed trait Input
  object Input {
    final case object FinishRecovery
    final case object SetReady extends Input
    final case class  StartStep(node: JobNode, jobActor: ActorRef) extends Input
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], event: OrderEvent)
  }
}
