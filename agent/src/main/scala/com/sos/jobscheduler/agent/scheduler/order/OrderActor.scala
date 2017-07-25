package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Status, Terminated}
import com.sos.jobscheduler.agent.scheduler.job.JobRunner
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskStepFailed, TaskStepSucceeded}
import com.sos.jobscheduler.agent.scheduler.order.OrderActor._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.Jobnet
import com.sos.jobscheduler.data.jobnet.Jobnet.JobNode
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.system.StdoutStderr
import com.sos.jobscheduler.data.system.StdoutStderr.StdoutStderrType
import com.sos.jobscheduler.shared.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.taskserver.task.process.StdoutStderrWriter
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Promise}

/**
  * @author Joacim Zschimmer
  */
private final class OrderActor(orderId: OrderId, protected val journalActor: ActorRef)
extends KeyedJournalingActor[OrderEvent] {

  private val logger = Logger.withPrefix[OrderActor](orderId.toString)
  private var order: Order[Order.State] = null
  private var terminating = false

  protected def key = orderId
  protected def snapshot = Option(order)

  protected def recoverFromSnapshot(snapshot: Any) = {
    assert(order == null)
    order = cast[Order[Order.State]](snapshot)
  }

  protected def recoverFromEvent(event: OrderEvent) = throw new NotImplementedError

  override protected def finishRecovery() = {
    assert(order != null, "No Order")
    order.state match {
      case Order.Waiting ⇒
        context.become(waiting)

      case Order.InProcess ⇒
        context.become(waiting)
        persist(OrderStepFailed(s"Agent aborted while order was InProcess", nextNodeId = order.nodeId))(update)

      case Order.StartNow | _: Order.Scheduled | Order.Ready | Order.Detached | Order.Finished ⇒
        context.become(waiting)

      case _ ⇒
    }
    logger.debug(s"Recovered $order")
    sender() ! Output.RecoveryFinished(order)
  }

  def receive = journaling orElse {
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
  }

  private val waiting: Receive = journaling orElse {
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
        jobActor ! JobRunner.Command.ProcessOrder(order.castAfterEvent(event), new MyStdoutStderrWriter(to = self))
      }

    case Input.SetReady ⇒
      persist(OrderReady)(update)

    case Input.Terminate ⇒
      context.stop(self)
  }

  private def processing(node: Jobnet.JobNode, jobActor: ActorRef): Receive = journaling orElse {
    case Internal.StdoutStderrWritten(t, chunk, promise) ⇒
      persistAsync(OrderStdWritten(t)(chunk)) { _ ⇒
        // TODO Sync oder flush ist hier nicht nötig und wird große Ausgaben verlangsamen. persisAsync(commit=false) ?
        promise.success(Completed)
      }

    case JobRunner.Response.OrderProcessed(`orderId`, moduleStepEnded) if node != null ⇒
      val event = moduleStepEnded match {
        case TaskStepSucceeded(variablesDiff, good) ⇒
          OrderStepSucceeded(variablesDiff, good.returnValue, nextNodeId(node, good))
        case TaskStepFailed(bad) ⇒
          OrderStepFailed(bad.error, nextNodeId(node, bad))
      }
      endOrderStep(event, node)
      context.unwatch(jobActor)

    case Terminated(`jobActor`) ⇒
      val bad = Order.Bad(s"Job Actor '${node.jobPath.string}' terminated unexpectedly")
      endOrderStep(OrderStepFailed(bad.error, nextNodeId = node.id/*nextNodeId(node, bad)*/), node)

    case command: Command ⇒
      executeOtherCommand(command)

    case Input.Terminate ⇒
      terminating = true
  }

  private def endOrderStep(event: OrderStepEnded, node: Jobnet.JobNode): Unit = {
    context.become(waiting)
    persist(event) { event ⇒
      update(event)
      if (terminating) {
        context.stop(self)
      }
    }
  }

  private def update(event: OrderEvent) = {
    updateOrder(event)
    context.parent ! Output.OrderChanged(order, event)
    if (event == OrderDetached) {
      context.stop(self)
    }
  }

  private def updateOrder(event: OrderEvent) = {
    order = event match {
      case event: OrderAttached ⇒
        Order.fromOrderAttached(orderId, event)
        // Order.state = Attached / MovedToAgent ???

      case OrderDetached ⇒
        logger.trace("Stopping after OrderDetached")
        order

      case _: OrderStdWritten ⇒
        // Not collected
        order

      case event: OrderCoreEvent if order != null ⇒
        order.update(event)

      case _ ⇒
        sys.error(s"Unexpected event for '$orderId': $event")
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
  private val StdoutStderrChunkSize = 10000  // Characters

  sealed trait Command
  object Command {
    final case class  Attach(order: Order[Order.Idle]) extends Command
    final case object Detach extends Command
  }

  sealed trait Input
  object Input {
    final case object SetReady extends Input
    final case class  StartStep(node: Jobnet.JobNode, jobActor: ActorRef) extends Input
    final case object Terminate extends Input
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], event: OrderEvent)
  }

  private object Internal {
    final case class StdoutStderrWritten(typ: StdoutStderrType, chunk: String, completed: Promise[Completed])
  }

  private def nextNodeId(node: JobNode, outcome: Order.Outcome) =
    outcome match {
      case Order.Good(returnValue) ⇒ if (returnValue) node.onSuccess else node.onFailure
      case Order.Bad(_) ⇒ node.onFailure
    }

  private class MyStdoutStderrWriter(to: ActorRef) extends StdoutStderrWriter {
    def chunkSize = StdoutStderrChunkSize

    def writeChunk(t: StdoutStderr.StdoutStderrType, chunk: String) = {
      val p = Promise[Completed]()
      to ! Internal.StdoutStderrWritten(t, chunk, p)
      Await.ready(p.future, Inf).successValue  // Blocks in stdout/stderr reader thread to avoid congestion
    }
  }
}
