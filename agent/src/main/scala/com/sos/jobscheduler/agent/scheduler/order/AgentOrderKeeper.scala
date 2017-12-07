package com.sos.jobscheduler.agent.scheduler.order

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.Tagger
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, AttachOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand, Response}
import com.sos.jobscheduler.agent.scheduler.event.EventQueueActor
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetached, OrderForked, OrderJoined, OrderProcessed}
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentAborted
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Workflow.EndNode
import com.sos.jobscheduler.data.workflow.WorkflowEvent.WorkflowAttached
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.JoinTransition
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow, WorkflowEvent}
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.startJournalAndFinishRecovery
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor, KeyedJournalingActor}
import com.sos.jobscheduler.shared.workflow.Transitions.ExecutableTransition
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.Duration
import scala.collection.immutable.{Iterable, Seq}
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * Keeper of one Master's orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  journalFile: Path,
  implicit private val askTimeout: Timeout,
  syncOnCommit: Boolean,
  keyedEventBus: StampedKeyedEventBus,
  eventIdGenerator: EventIdGenerator,
  config: Config,
  implicit private val timerService: TimerService)
extends KeyedEventJournalingActor[WorkflowEvent] with Stash {

  import context.{actorOf, dispatcher, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor = actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val orderRegister = new OrderRegister(timerService)
  private val eventsForMaster = actorOf(Props { new EventQueueActor(timerService) }, "eventsForMaster").taggedWith[EventQueueActor]

  private var terminating = false

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  private def recover(): Unit = {
    val recoverer = new OrderJournalRecoverer(journalFile = journalFile, eventsForMaster)(askTimeout)
    recoverer.recoverAll()
    for (workflow ← recoverer.workflows)
      wrapException(s"Error when recovering ${workflow.path}") {
        workflowRegister.recover(workflow)
      }
    for (recoveredOrder ← recoverer.orders)
      wrapException(s"Error when recovering ${recoveredOrder.id}") {
        val order = workflowRegister.reuseMemory(recoveredOrder)
        val workflow = workflowRegister(order.workflowPath)  // Workflow must be recovered
        val actor = newOrderActor(order)
        orderRegister.recover(order, workflow, actor)
        actor ! KeyedJournalingActor.Input.Recover(order)
      }
    startJournalAndFinishRecovery(journalActor = journalActor, orderRegister.recoveredJournalingActors)
  }

  override def postStop() = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def snapshots = {
    val workflowSnapshots = workflowRegister.workflows
    for (got ← (eventsForMaster ? EventQueueActor.Input.GetSnapshots).mapTo[EventQueueActor.Output.GotSnapshots])
      yield workflowSnapshots ++ got.snapshots  // Future: don't use mutable `this`
  }

  def receive = journaling orElse {
    case Input.Start(jobPathsAndActors) ⇒
      for ((jobPath, actorRef) ← jobPathsAndActors) {
        jobRegister.insert(jobPath, actorRef)
        watch(actorRef)
      }
      context.become(awaitJournalIsReady)
      unstashAll()

    case _ ⇒
      stash()  // We stash all early OrderActor.Output.RecoveryFinished until the jobs are defined (Input.Start)
  }

  private def awaitJournalIsReady: Receive = journaling orElse {
    case OrderActor.Output.RecoveryFinished(order) ⇒
      orderRegister(order.id).order = order
      proceedWithOrder(order.id)

    case JsonJournalRecoverer.Output.JournalIsReady ⇒
      logger.info(s"${workflowRegister.size} Workflows recovered, ${orderRegister.size} Orders recovered")
      context.become(ready)
      unstashAll()
      logger.info("Ready")

    case _ ⇒
      stash()
  }

  private def ready: Receive = journaling orElse {
    case Input.ExternalCommand(cmd, response) ⇒
      response.completeWith(processOrderCommand(cmd))

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      eventsForMaster.forward(EventQueueActor.Input.RequestEvents(after, timeout, limit, promise))

    case Input.Terminate ⇒
      if (!terminating) {
        terminating = true
        for (o ← orderRegister.values if !o.detaching) {
          o.actor ! OrderActor.Input.Terminate
        }
      }
      eventsForMaster ! PoisonPill
      checkActorStop()
      sender() ! Done

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      handleOrderEvent(order, event)

    case JobActor.Output.ReadyForOrder if (jobRegister contains sender()) && !terminating ⇒
      tryStartProcessing(jobRegister(sender()))

    case Internal.ContinueAttachOrder(cmd @ AttachOrder(order, workflow), promise) ⇒
      import order.nodeKey
      promise completeWith {
        if (!workflow.isDefinedAt(nodeKey.nodeId))
          Future.failed(new IllegalArgumentException(s"Unknown NodeId ${nodeKey.nodeId} in ${nodeKey.workflowPath}"))
        else
          if (orderRegister contains order.id) {
            // May occur after Master restart when Master is not sure about order has been attached previously.
            logger.debug(s"Ignoring duplicate $cmd")
            Future.successful(Accepted)
          } else
            attachOrder(workflowRegister.reuseMemory(order), workflow) map { case Completed ⇒ Accepted }
      }

    case Internal.Due(orderId) if orderRegister contains orderId ⇒
      val orderEntry = orderRegister(orderId)
      onOrderAvailable(orderEntry)

    case stamped @ Stamped(_, KeyedEvent(_: OrderId, event: OrderEvent)) if !terminating ⇒
      event match {
        case OrderDetached ⇒
          (eventsForMaster ? stamped) await 2 * askTimeout.duration.toJavaDuration  // blocking !!!
        case _ ⇒
          eventsForMaster ! stamped
      }
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Response] = cmd match {
    case cmd @ AttachOrder(order, workflow) if !terminating ⇒
      order.attachedToAgent match {
        case Left(throwable) ⇒ Future.failed(throwable)
        case Right(_) ⇒
          val workflowResponse = workflowRegister.get(workflow.path) match {
            case None ⇒
              persistFuture(KeyedEvent(WorkflowAttached(workflow))(workflow.path)) { stampedEvent ⇒
                workflowRegister.handleEvent(stampedEvent.value)
                Accepted
              }
            case Some(`workflow`) ⇒
              Future.successful(Accepted)
            case Some(_) ⇒
              Future.failed(new IllegalStateException(s"Changed ${workflow.path}"))
          }
          workflowResponse flatMap { case Accepted ⇒
            promiseFuture[Accepted.type] { promise ⇒
              self ! Internal.ContinueAttachOrder(cmd, promise)
            }
          }
      }

    case DetachOrder(orderId) ⇒
      orderRegister.get(orderId) match {
        case Some(orderEntry) ⇒
          orderEntry.order.detachableFromAgent match {
            case Left(throwable) ⇒ Future.failed(throwable)
            case Right(_) ⇒
              orderEntry.detaching = true  // OrderActor is terminating
              (orderEntry.actor ? OrderActor.Command.Detach).mapTo[Completed] map { _ ⇒ Accepted }  // TODO ask will time-out when Journal blocks
          }
        case None ⇒
          // May occur after Master restart when Master is not sure about order has been detached previously.
          logger.debug(s"Ignoring duplicate $cmd")
          Future.successful(Accepted)
      }

    case GetOrder(orderId) ⇒
      executeCommandForOrderId(orderId) { orderEntry ⇒
        Future.successful(GetOrder.Response(
          orderEntry.order))
      }

    case GetOrderIds ⇒
      Future.successful(GetOrderIds.Response(
        orderRegister.keys))

    case GetOrders ⇒
      Future.successful(GetOrders.Response(
        for (orderEntry ← orderRegister.values) yield orderEntry.order))

    case _ if terminating ⇒
      Future.failed(new IllegalStateException(s"Agent is terminating"))
  }

  private def executeCommandForOrderId(orderId: OrderId)(body: OrderEntry ⇒ Future[Response]): Future[Response] =
    orderRegister.get(orderId) match {
      case Some(orderEntry) ⇒
        body(orderEntry)
      case None ⇒
        Future.failed(new IllegalArgumentException(s"Unknown $orderId"))
    }

  private def attachOrder(order: Order[Order.Idle], workflow: Workflow): Future[Completed] = {
    val actor = newOrderActor(order)
    orderRegister.insert(order, workflow, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttached
  }

  private def newOrderActor(order: Order[Order.State]) =
    watch(actorOf(
      Props { new OrderActor(order.id, journalActor = journalActor, config) },
      name = encodeAsActorName(s"Order-${order.id.string}")))

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent): Unit = {
    val orderEntry = orderRegister(order.id)
    val previousOrder = orderEntry.order
    orderEntry.order = order
    event match {
      case event: OrderProcessed if event.outcome != OrderActor.RecoveryGeneratedOutcome ⇒
        assert(order.state == Order.Processed)
        val jobPath = orderEntry.jobNode.jobPath
        for (jobEntry ← jobRegister.get(jobPath))  // JobActor may be stopped
          jobEntry.queue -= order.id

      case OrderForked(children) ⇒
        for (child ← children) {
          val childOrder = order.newChild(child)
          val actor = newOrderActor(childOrder)
          orderRegister.insert(childOrder, orderEntry.workflow, actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder.id)
        }

      case joined: OrderJoined ⇒
        previousOrder.state match {
          case Order.Forked(childOrderIds) ⇒
            for (childOrderId ← childOrderIds) {
              orderRegister(childOrderId).actor ! OrderActor.Input.DeleteChild
            }
          case state ⇒
            logger.error(s"Event $joined, but Order is in state $state")
        }

      case _ ⇒
    }
    proceedWithOrder(order.id)
  }

  private def proceedWithOrder(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    if (orderEntry.order.isAttachedToAgent) {
      orderEntry.order.state match {
        case Order.Scheduled(instant) if now < instant ⇒
          orderEntry.at(instant) {  // TODO Register only the next order in TimerService ?
            self ! Internal.Due(orderId)
          }

        case _: Order.Idle ⇒
          onOrderAvailable(orderEntry)

        case Order.Processed ⇒
          handleProcessed(orderEntry)

        case _ ⇒
      }
    }
  }

  private def onOrderAvailable(orderEntry: OrderEntry): Unit =
    orderEntry.order.attachedToAgent match {
      case Left(throwable) ⇒ logger.error(s"onOrderAvailable: $throwable")
      case Right(_) ⇒
        orderEntry.nodeOption match {
          case Some(node: Workflow.JobNode) if !terminating ⇒
            jobRegister.get(node.jobPath) match {
              case Some(jobEntry) ⇒
                onOrderAvailableForJob(orderEntry.order.id, jobEntry)
              case None ⇒
                logger.error(s"Missing '${node.jobPath}' for '${orderEntry.order.id}' at '${orderEntry.order.nodeKey}'")
            }

          case Some(node: Workflow.JobNode) if terminating ⇒
            logger.info(s"Due to termination, processing of ${orderEntry.order.id} stops at ${node.id}")

          case Some(_: EndNode) | None ⇒
            if (!terminating) {  // When terminating, the order actors are terminating now
              logger.trace(s"${orderEntry.order.id} is detachable, ready to be retrieved by the Master")
              orderRegister(orderEntry.order.id).actor ! OrderActor.Input.MakeDetachable
            }
        }
    }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit = {
    logger.trace(s"$orderId is queuing for ${jobEntry.jobPath}")
    jobEntry.queue += orderId
    if (jobEntry.waitingForOrder) {
      tryStartProcessing(jobEntry)
    } else {
      jobEntry.actor ! JobActor.Input.OrderAvailable
    }
  }

  private def tryStartProcessing(jobEntry: JobEntry): Unit =
    jobEntry.queue.dequeue() match {
      case Some(orderId) ⇒
        orderRegister.get(orderId) match {
          case None ⇒
            logger.warn(s"Unknown $orderId was enqueued for ${jobEntry.jobPath}. Order has been removed?")  // TODO Why can this happen?

          case Some(orderEntry) ⇒
            orderEntry.nodeOption match {
              case Some(node: Workflow.JobNode) ⇒
                startProcessing(orderEntry, node, jobEntry)
              case _ ⇒
                logger.error(s"${orderEntry.order.id}: ${orderEntry.order.nodeKey} does not denote a JobNode")
            }
            jobEntry.waitingForOrder = false
        }

      case None ⇒
        jobEntry.waitingForOrder = true
    }

  private def startProcessing(orderEntry: OrderEntry, node: Workflow.JobNode, jobEntry: JobEntry): Unit = {
    logger.trace(s"${orderEntry.order.id} is going to be processed by ${jobEntry.jobPath}")
    assert(node.jobPath == jobEntry.jobPath)
    jobEntry.waitingForOrder = false
    orderEntry.actor ! OrderActor.Input.StartProcessing(node, jobEntry.actor)
  }

  private def handleProcessed(orderEntry: OrderEntry): Unit = {
    val order = orderEntry.order.castState[Order.Processed.type]
    order.outcome match {
      case Outcome.Bad(AgentAborted) ⇒
        orderEntry.actor ! OrderActor.Input.HandleTransitionEvent(OrderEvent.OrderMoved(order.nodeId))  // Repeat

      case _ ⇒
        orderEntry.workflow.nodeToTransition.get(order.nodeId) match {
          case None ⇒
            orderEntry.actor ! OrderActor.Input.MakeDetachable

          case Some(transition) ⇒
            Try {
              transition.switch(ordersForTransition(transition, order).toKeyedMap(_.nodeId) withNoSuchKey (k ⇒ s"Unknown NodeId '$k' for $transition, ${order.id}"))
            } match {
              case Failure(t) ⇒
                logger.warn(s"Error when applying $transition to '${order.id}' ${t.toStringWithCauses}", t)
              case Success(Some(keyedEvent)) ⇒
                orderRegister(keyedEvent.key).actor ! OrderActor.Input.HandleTransitionEvent(keyedEvent.event)
              case Success(None) ⇒
                // JoinTransition? Await for more input orders
            }
        }
    }
  }

  private def ordersForTransition(transition: Transition, order: Order[Order.Processed.type]): Iterable[Order[Order.Transitionable]] =
    transition.transitionType match {
      case JoinTransition ⇒
        val parentOrderId = order.parent.getOrElse(throw new RuntimeException("No parent"))
        val parent = orderRegister(parentOrderId).order.castState[Order.Forked]
        val siblings = for {
          siblingOrderId ← parent.state.childOrderIds
          sibling ← orderRegister(siblingOrderId).order.ifState[Order.Processed.type]
          if transition.fromNodeIds contains sibling.nodeId
        } yield sibling
        parent +: siblings

      case _ ⇒
        order :: Nil
    }

  override def unhandled(message: Any) =
    message match {
      case Terminated(actorRef) if jobRegister contains actorRef ⇒
        val jobPath = jobRegister.actorToKey(actorRef)
        if (terminating) {
          logger.debug(s"Actor $jobPath has stopped")
        } else {
          logger.error(s"Actor '$jobPath' has stopped unexpectedly")
        }
        jobRegister.onActorTerminated(actorRef)
        checkActorStop()

      case Terminated(actorRef) if orderRegister contains actorRef ⇒
        val orderId = orderRegister(actorRef).order.id
        logger.debug(s"Actor '$orderId' stopped")
        orderRegister.onActorTerminated(actorRef)
        checkActorStop()

      case _ ⇒
        super.unhandled(message)
    }

  private def checkActorStop() = {
    if (terminating && orderRegister.isEmpty && jobRegister.isEmpty) {
      context.stop(self)
    }
  }

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[Workflow],
    Subtype[Order[Order.State]],
    Subtype[EventQueueActor.Snapshot])

  private[order] val MyJournalMeta = new JsonJournalMeta[Event](SnapshotJsonFormat, AgentKeyedEventJsonCodec) with GzipCompression

  sealed trait Input
  object Input {
    final case class Start(jobs: Seq[(JobPath, ActorRef)]) extends Input
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[EventQueueActor.MyEventSeq]) extends Input
    final case class ExternalCommand(command: OrderCommand, response: Promise[Response])
    final case object Terminate
  }

  private object Internal {
    final case class ContinueAttachOrder(cmd: AgentCommand.AttachOrder, promise: Promise[Accepted.type])
    final case class Due(orderId: OrderId)
  }
}
