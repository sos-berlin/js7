package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Cancellable, Status, Terminated}
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.{TaskStepFailed, TaskStepSucceeded}
import com.sos.jobscheduler.agent.scheduler.order.OrderActor._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.system.StdoutStderr.{Stderr, Stdout, StdoutStderrType}
import com.sos.jobscheduler.data.workflow.{NodeId, Workflow}
import com.sos.jobscheduler.shared.event.journal.KeyedJournalingActor
import com.sos.jobscheduler.taskserver.task.process.StdChannels
import com.typesafe.config.Config
import java.time.Duration
import scala.concurrent.Promise
import scala.math.min

/**
  * @author Joacim Zschimmer
  */
final class OrderActor(orderId: OrderId, protected val journalActor: ActorRef, config: Config)
extends KeyedJournalingActor[OrderEvent] {

  private val logger = Logger.withPrefix[OrderActor](orderId.toString)

  private var order: Order[Order.State] = null
  private val stdCharBlockSize = config.getInt("jobscheduler.agent.task.stdout-event-size")  // See also StdPipeCharBufferSize
  private val delay = config.as[Duration]("jobscheduler.agent.task.stdout-delay")
  private val stdWriters = Map(
    Stdout → new StdWriter(Stdout, self, size = stdCharBlockSize, passThroughSize = stdCharBlockSize / 2, delay),
    Stderr → new StdWriter(Stderr, self, size = stdCharBlockSize, passThroughSize = stdCharBlockSize / 2, delay))
  private var stdoutStderrTimer: Cancellable = null
  private var terminating = false

  protected def key = orderId
  protected def snapshot = Option(order)

  override def postStop() = {
    cancelStdoutStderrTimer()
    super.postStop()
  }

  protected def recoverFromSnapshot(snapshot: Any) = {
    assert(order == null)
    order = cast[Order[Order.State]](snapshot)
  }

  protected def recoverFromEvent(event: OrderEvent) = throw new NotImplementedError

  override protected def finishRecovery() = {
    assert(order != null, "No Order")
    order.state match {
      case Order.Ready ⇒
        context.become(waiting)

      case Order.InProcess ⇒
        context.become(processed)
        persist(OrderProcessed(MapDiff.empty, Outcome.Bad(Outcome.Bad.AgentAborted)))(update)  // isRecoveryGeneratedEvent

      case Order.Processed ⇒
        context.become(processed)
        // Next event 'OrderTransitioned' is initiated by AgentOrderKeeper

      case Order.StartNow | _: Order.Scheduled | Order.Finished ⇒
        context.become(waiting)

      case _ ⇒
    }
    logger.debug(s"Recovered $order")
    sender() ! Output.RecoveryFinished(order)  // Sent via JsonJournalRecoverer to AgentOrderKeeper
  }

  def receive = journaling orElse {
    case command: Command ⇒ command match {
      case Command.Attach(Order(`orderId`, nodeKey, state: Order.Idle, Some(Order.AttachedTo.Agent(agentPath)), payload)) ⇒
        context.become(waiting)
        persist(OrderAttached(nodeKey, state, agentPath, payload)) { event ⇒
          sender() ! Completed
          update(event)
        }

      case _ ⇒
        executeOtherCommand(command)
    }
  }

  private val waiting: Receive = journaling orElse {
    case Command.Detach ⇒
      persist(OrderDetached) { event ⇒
        sender() ! Completed
        update(event)
      }

    case command: Command ⇒
      executeOtherCommand(command)

    case input: Input ⇒
      executeInput(input)
  }

  private def executeOtherCommand(command: Command): Unit = command match {
    case _ ⇒
      val msg = s"Improper command $command while in state ${Option(order) map (_.state) getOrElse "(no order)"}"
      logger.error(msg)
      sender() ! Status.Failure(new IllegalStateException(msg))
  }

  private def executeInput(input: Input) = input match {
    case Input.StartProcessing(jobNode, jobActor) ⇒
      val stdoutWriter = new StatisticalWriter(stdWriters(Stdout))
      val stderrWriter = new StatisticalWriter(stdWriters(Stderr))
      context.become(processing(jobNode, jobActor,
        () ⇒ (stdoutWriter.nonEmpty || stderrWriter.nonEmpty) option s"stdout: $stdoutWriter, stderr: $stderrWriter"))
      context.watch(jobActor)
      persist(OrderProcessingStarted) { event ⇒
        update(event)
        jobActor ! JobActor.Command.ProcessOrder(
          order.castAfterEvent(event),
          new StdChannels(
            charBufferSize = min(StdPipeCharBufferSize, stdCharBlockSize),
            stdoutWriter = stdoutWriter,
            stderrWriter = stderrWriter))
      }

    case Input.MakeDetachable ⇒
      persist(OrderDetachable)(update)

    case Input.Terminate ⇒
      context.stop(self)
  }

  private def processing(node: Workflow.JobNode, jobActor: ActorRef, stdoutStderrStatistics: () ⇒ Option[String]): Receive =
    journaling orElse {
      case Internal.StdoutStderrWritten(t, chunk, promise) ⇒
        persistAsync(OrderStdWritten(t)(chunk)) { _ ⇒
          promise.success(Completed)
        }

      case Internal.BufferingStarted ⇒
        if (stdoutStderrTimer == null) {
          stdoutStderrTimer = context.system.scheduler.scheduleOnce(
            delay.toFiniteDuration, context.self, Internal.FlushStdoutStderr)(context.dispatcher)
        }

      case Internal.FlushStdoutStderr ⇒
        flushStdoutAndStderr()
        stdoutStderrTimer = null

      case JobActor.Response.OrderProcessed(`orderId`, moduleStepEnded) ⇒
        val event = moduleStepEnded match {
          case TaskStepSucceeded(variablesDiff, good) ⇒
            OrderProcessed(variablesDiff, good)

          case TaskStepFailed(bad) ⇒
            OrderProcessed(MapDiff.empty, bad)
        }
        finishProcessing(event, node, stdoutStderrStatistics)
        context.unwatch(jobActor)

      case Terminated(`jobActor`) ⇒
        val bad = Outcome.Bad(Outcome.Bad.Other(s"Job Actor '${node.jobPath.string}' terminated unexpectedly"))
        finishProcessing(OrderProcessed(MapDiff.empty, bad), node, stdoutStderrStatistics)

      case command: Command ⇒
        executeOtherCommand(command)

      case Input.Terminate ⇒
        terminating = true
    }

  private def finishProcessing(event: OrderProcessed, node: Workflow.JobNode, stdoutStderrStatistics: () ⇒ Option[String]): Unit = {
    flushStdoutAndStderr()
    cancelStdoutStderrTimer()
    for (o ← stdoutStderrStatistics()) logger.debug(o)
    context.become(processed)
    persist(event) { event ⇒
      update(event)
      if (terminating) {
        context.stop(self)
      }
    }
  }

  private def flushStdoutAndStderr(): Unit =
    for (o ← stdWriters.values) o.flush()

  private def cancelStdoutStderrTimer(): Unit = {
    if (stdoutStderrTimer != null) {
      stdoutStderrTimer.cancel()
      stdoutStderrTimer = null
    }
  }

  private def processed: Receive = journaling orElse {
    case Input.Transition(toNodeId) ⇒
      context.become(waiting)
      persist(OrderTransitioned(toNodeId))(update)

    case command: Command ⇒
      executeOtherCommand(command)
  }

  private def update(event: OrderEvent) = {
    updateOrder(event)
    context.parent ! Output.OrderChanged(order, event)
    if (event == OrderDetached) {
      logger.trace("Stopping after OrderDetached")
      context.stop(self)
    }
  }

  private def updateOrder(event: OrderEvent) = {
    order = event match {
      case event: OrderAttached ⇒
        Order.fromOrderAttached(orderId, event)
        // Order.state = Attached / MovedToAgent ???

      case OrderDetached ⇒
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
  private val StdPipeCharBufferSize = 500  // Even a size of 1 character is not slow

  sealed trait Command
  object Command {
    final case class  Attach(order: Order[Order.Idle]) extends Command
    final case object Detach extends Command
  }

  sealed trait Input
  object Input {
    final case object MakeDetachable extends Input
    final case class  StartProcessing(node: Workflow.JobNode, jobActor: ActorRef) extends Input
    final case object Terminate extends Input
    final case class Transition(toNodeId: NodeId)
  }

  object Output {
    final case class RecoveryFinished(order: Order[Order.State])
    final case class OrderChanged(order: Order[Order.State], event: OrderEvent)
  }

  private object Internal {
    final case object BufferingStarted
    final case class StdoutStderrWritten(typ: StdoutStderrType, chunk: String, completed: Promise[Completed])
    final case object FlushStdoutStderr
  }

  private class StdWriter(stdoutOrStderr: StdoutStderrType, self: ActorRef, protected val size: Int, protected val passThroughSize: Int, delay: Duration)
  extends BufferedStringWriter {

    def close() = flush()

    protected def onFlush(string: String) = {
      val promise = Promise[Completed]()
      self ! Internal.StdoutStderrWritten(stdoutOrStderr, string, promise)
      promise.future
    }

    protected def onBufferingStarted() =
      self ! Internal.BufferingStarted
  }

  def isRecoveryGeneratedEvent(event: OrderEvent): Boolean =
    event match {
      case OrderProcessed(_, Outcome.Bad(Outcome.Bad.AgentAborted)) ⇒ true
      case _ ⇒ false
    }
}
