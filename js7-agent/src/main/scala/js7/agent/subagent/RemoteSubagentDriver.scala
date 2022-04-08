package js7.agent.subagent

import akka.actor.ActorSystem
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.typesafe.config.ConfigUtil
import js7.agent.data.AgentState
import js7.agent.data.Problems.SubagentNotDedicatedProblem
import js7.agent.subagent.RemoteSubagentDriver._
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.generic.SecretString
import js7.base.io.https.HttpsConfig
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.{AsyncMap, AsyncVariable, Switch}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.stream.Numbered
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.controller.ControllerId
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.event.EventId
import js7.data.item.{InventoryItemKey, ItemRevision, SignableItem}
import js7.data.job.{JobConf, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentItemStateEvent.{SubagentDedicated, SubagentDied, SubagentRestarted}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentRunId}
import js7.data.workflow.Workflow
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.StatePersistence
import js7.subagent.client.{SubagentClient, SubagentDriver}
import js7.subagent.configuration.SubagentConf
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, KillProcess, StartOrderProcess}
import monix.catnap.MVar
import monix.eval.Task
import scala.annotation.unused
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.util.{Failure, Success}

private final class RemoteSubagentDriver(
  val subagentItem: SubagentItem,
  httpsConfig: HttpsConfig,
  protected val persistence: StatePersistence[AgentState],
  controllerId: ControllerId,
  protected val conf: SubagentDriver.Conf,
  protected val subagentConf: SubagentConf,
  protected val recouplingStreamReaderConf: RecouplingStreamReaderConf,
  actorSystem: ActorSystem)
extends SubagentDriver with SubagentEventListener
{
  protected type S = AgentState

  private val logger = Logger.withPrefix[this.type](subagentItem.pathRev.toString)
  private val dispatcher = new SubagentDispatcher(subagentId, postQueuedCommand)
  private val attachedItemKeys = AsyncVariable(Map.empty[InventoryItemKey, Option[ItemRevision]])
  private val initiallyCoupled = SetOnce[SubagentRunId]
  @volatile private var lastSubagentRunId: Option[SubagentRunId] = None
  @volatile private var stopping = false
  private val stoppingVar = MVar.empty[Task, Unit]().memoize
  @volatile private var shuttingDown = false

  def subagentId = subagentItem.id

  override def isCoupled =
    super.isCoupled &&
      isHeartbeating &&
      persistence.currentState
        .idToSubagentItemState.get(subagentId)
        .exists(s => s.couplingState == Coupled
          /*Due to isHeartbeating we ignore s.problem to allow SubagentCoupled event.*/)

  def isStopping = stopping

  def isShuttingDown = shuttingDown

  protected val client = new SubagentClient(
    Admission(
      subagentItem.uri,
      subagentConf.config
        .optionAs[SecretString](
          "js7.auth.subagents." + ConfigUtil.joinPath(subagentItem.id.string))
        .map(UserAndPassword(subagentItem.agentPath.toUserId.orThrow, _))),
    httpsConfig,
    name = subagentItem.id.toString,
    actorSystem)

  private val orderToPromise = AsyncMap.stoppable[OrderId, Promise[OrderProcessed]]()

  val start: Task[Unit] =
    logger
      .debugTask(
        dedicateOrCouple
          .map(_.orThrow)
          .flatMap { case (subagentRunId, _) =>
            startEventListener *> dispatcher.start(subagentRunId)
          })
      .memoize

  def startMovedSubagent(previous: RemoteSubagentDriver): Task[Unit] =
    logger.debugTask(
      startEventListener
        //.*>(previous.emitProcessLostEvents(None))
        .*>(Task.race(
          stoppingVar.flatMap(_.read),
          initiallyCoupled.task))
        .flatMap {
          case Left(())/*stopped*/ => Task.unit
          case Right(subagentRunId) =>
            logger.debug(s"startMovedSubagent(${previous.lastSubagentRunId} $previous ${previous.hashCode}): this=$subagentRunId")
            // Does not work, so we kill all processes. FIXME Do we kill them?
            //if (previous.lastSubagentRunId contains subagentRunId)
            //  // Same SubagentRunId continues. So we transfer the command queue.
            //  dispatcher.enqueueExecutes(previous.dispatcher)
            //else
              /*previous.emitProcessLostEvents(None)
            .*>*/(dispatcher.start(subagentRunId))
        })

  def stop(@unused signal: Option[ProcessSignal]): Task[Unit] =
    stop

  val stop: Task[Unit] =
    logger
      .debugTask(
        stoppingVar.flatMap(_.tryPut(()))
          .*>(Task.defer {
            stopping = true
            Task.parZip2(dispatcher.stop(), stopEventListener)
              .*>(client.tryLogout.void)
              .logWhenItTakesLonger(s"RemoteSubagentDriver($subagentId).stop")
          }))
      .memoize

  def tryShutdown: Task[Unit] =
    logger.debugTask(Task.defer {
      shuttingDown = true
      // Wait until no Order is being processed
      orderToPromise.stop
        // Emit event and change state ???
        .*>(tryShutdownSubagent)
    })

  //def suspend: Task[Unit] =
  //  dispatcher.suspend *> stopEventListener

  private def tryShutdownSubagent: Task[Unit] =
    client.executeSubagentCommand(Numbered(0, SubagentCommand.ShutDown(restart = true)))
      .void
      .onErrorHandle(t =>  // Ignore when Subagent is unreachable
        logger.error(s"SubagentCommand.ShutDown => ${t.toStringWithCauses}"))

  protected def dedicateOrCouple: Task[Checked[(SubagentRunId, EventId)]] =
    logger.debugTask(
      currentSubagentItemState
        .flatMapT(subagentItemState =>
          dedicateOrCouple2(subagentItemState).map(Right(_))))

  private def dedicateOrCouple2(subagentItemState: SubagentItemState): Task[(SubagentRunId, EventId)] =
    subagentItemState.subagentRunId match {
      case None =>
        for {
          response <- dedicate
          eventId <- couple(response.subagentRunId, response.subagentEventId)
        } yield (response.subagentRunId, eventId)

      case Some(subagentRunId) =>
        couple(subagentRunId, subagentItemState.eventId)
          .map(subagentRunId -> _)
    }

  private def dedicate: Task[DedicateSubagent.Response] = {
    val cmd = DedicateSubagent(subagentId, subagentItem.agentPath, controllerId)
    logger.debugTask(
      postCommandUntilSucceeded(cmd)
        .flatMap(response => persistence
          .persistKeyedEvent(subagentId <-: SubagentDedicated(response.subagentRunId))
          .tapEval(checked => Task.when(checked.isRight)(Task {
            lastSubagentRunId = Some(response.subagentRunId)
          }))
          .rightAs(response)
          .map(_.orThrow)
          .<*(dispatcher.start(response.subagentRunId))))
  }

  private def couple(subagentRunId: SubagentRunId, eventId: EventId): Task[EventId] = {
    val cmd = CoupleDirector(subagentId, subagentRunId, eventId,
      SubagentEventListener.heartbeatTiming)
    Task
      .tailRecM(())(_ =>
        // TODO Must be stoppable
        postCommand(Numbered(0, cmd))
          .as(Right(subagentRunId -> eventId))
          .onErrorRecoverWith {
            case HttpException.HasProblem(SubagentNotDedicatedProblem) =>
              orderToPromise.toMap.size match {
                case 0 => logger.info("Subagent restarted")
                case n => logger.warn(s"Subagent restarted, $n Order processes are lost")
              }
              onSubagentDied(SubagentRestarted)
                .*>(dedicate)
                .map(response => Right(response.subagentRunId -> response.subagentEventId))

            case throwable =>
              logger.warn(s"DedicateSubagent failed: ${throwable.toStringWithCauses}")
              Task.left(()).delayExecution(reconnectErrorDelay)
          })
      .flatTap { case (subagentRunId, _) =>
        Task {
          lastSubagentRunId = Some(subagentRunId)
          initiallyCoupled.trySet(subagentRunId)
        }
      }
      .map(_._2/*EventId*/)
  }

  // May run concurrently with onStartOrderProcessFailed !!!
  // We make sure that only one OrderProcessed event is emitted.
  /** Emit OrderProcessed(ProcessLost) and `subagentDied` events. */
  protected def onSubagentDied(subagentDied: SubagentDied): Task[Unit] =
    emitProcessLostEvents(Some(subagentDied))

  /** Emit OrderProcessed(ProcessLost) and optionally a `subagentDied` event. */
  def emitProcessLostEvents(subagentDied: Option[SubagentDied]): Task[Unit] = {
    // Subagent died and lost its state
    // Emit OrderProcessed(Disrupted(ProcessLost)) for each processing order.
    // Then optionally subagentDied
    val processing = Order.Processing(subagentId)
    logger.debugTask(orderToPromise
      .removeAll
      .flatMap { oToP =>
        val orderIds = oToP.keys
        val promises = oToP.values
        Task
          .when(subagentDied.isDefined)(
            dispatcher.stop {
              case _: StartOrderProcess => CommandDispatcherStoppedProblem
            })
          .*>(attachedItemKeys.update(_ => Task.pure(Map.empty)))
          .*>(persistence
            .persist(state => Right(orderIds.view
              .filter(orderId =>
                // Just to be sure, condition should always be true:
                state.idToOrder.get(orderId).exists(_.state == processing))
              // Filter Orders which have been sent to Subagent ???
              .map(_ <-: OrderProcessed.processLost)
              .concat(subagentDied.map(subagentId <-: _))
              .toVector))
            .map(_.orThrow)
            .*>(Task {
              for (p <- promises) p.success(OrderProcessed.processLost)
            }))
      })
  }

  // May run concurrently with onSubagentDied !!!
  // Be sure that only on OrderProcessed event is emitted!
  private def onStartOrderProcessFailed(startOrderProcess: StartOrderProcess): Task[Checked[Unit]] =
    persistence
      .persist(state => Right(
        state.idToOrder.get(startOrderProcess.orderId)
          .filter(o => // Just to be sure, condition should always be true:
            o.state == Order.Processing(subagentId) &&
              o.workflowPosition == startOrderProcess.order.workflowPosition)
          .map(_.id <-: OrderProcessed.processLost)
          .toList))
      .rightAs(())

  /** Continue a recovered processing Order. */
  def continueProcessingOrder(order: Order[Order.Processing]) =
    processOrder(order)

  def processOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]] =
    logger.traceTask("processOrder", order.id)(
      ifNotStopping.flatMapT(_ => runProcessingOrder(order)))

  private def runProcessingOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]] =
    orderToPromise
      .insert(order.id, Promise())
      // OrderProcessed event will fulfill and remove the promise
      .flatMapT(promisedOutcome =>
        orderToExecuteDefaultArguments(order)
          .map(_.map(StartOrderProcess(order, _)))
          .flatMapT(dispatcher.executeCommand)
          .materializeIntoChecked
          .flatMap {
            case Left(problem) =>
              // StartOrderProcesss failed
              orderToPromise
                .remove(order.id)
                .flatMap {
                  case None =>
                    // Promise has been removed
                    if (problem != CommandDispatcherStoppedProblem) {
                      // onSubagentDied has stopped all queued StartOrderProcess commands
                      logger.warn(s"${order.id} got OrderProcessed, so we ignore $problem")
                    }
                    // Wait for promise
                    Task.fromFuture(promisedOutcome.future)
                      .map(Right(_))

                  case Some(promise) =>
                    val orderProcessed = OrderProcessed(Outcome.Disrupted(problem))
                    persistence
                      .persistKeyedEvent(order.id <-: orderProcessed)
                      .rightAs(orderProcessed)
                      .<*(Task { promise.success(orderProcessed) })
                }

            case Right(()) =>
              // Command succeeded, wait for promise
              Task.fromFuture(promisedOutcome.future)
                .map(Right(_))
          })

  protected def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed)
  : Task[Option[Task[Unit]]] =
    orderToPromise.remove(orderId).map {
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")
        None

      case Some(processing) =>
        Some(Task(processing.success(orderProcessed)))
    }

  private def killAll(signal: ProcessSignal): Task[Unit] =
    Task.defer {
      val cmds = orderToPromise.toMap.keys.map(KillProcess(_, signal))
      dispatcher
        .executeCommands(cmds)
        .map(cmds.zip(_).map {
          case (cmd, Left(problem)) => logger.error(s"$cmd => $problem")
          case _ =>
        })
    }

  def killProcess(orderId: OrderId, signal: ProcessSignal) =
    dispatcher.executeCommand(KillProcess(orderId, signal))
      .map {
        // TODO Stop postQueuedCommand loop for this OrderId
        // Subagent may have been restarted
        case Left(problem) => logger.error(s"killProcess $orderId => $problem")
        case Right(_) =>
      }

  private def ifNotStopping: Task[Checked[Unit]] =
    Task {
      !stopping !! Problem.pure(s"RemoteSubagentDriver $subagentId is stopping")
    }

  // TODO How to cancel this ?
  private def postCommandUntilSucceeded(command: SubagentCommand): Task[command.Response] =
    postCommand(Numbered(0, command))
      .map(_.asInstanceOf[command.Response])
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.error(
          s"${command.getClass.simpleScalaName} command failed: ${throwable.toStringWithCauses}")
        retry(()).delayExecution(5.s/*TODO*/)
      }

  private def postCommand(numberedCommand: Numbered[SubagentCommand])
  : Task[SubagentCommand.Response] = {
    val command = numberedCommand.value
    logger.traceTask("postCommand", command.toShortString)(
      client
        .retryUntilReachable()(
          client.executeSubagentCommand(numberedCommand)))
  }

  protected def detachProcessedOrder(orderId: OrderId): Task[Unit] =
    enqueueCommandAndForget(
      SubagentCommand.DetachProcessedOrder(orderId))

  protected def releaseEvents(eventId: EventId): Task[Unit] =
    enqueueCommandAndForget(
      SubagentCommand.ReleaseEvents(eventId))

  private def enqueueCommandAndForget(cmd: SubagentCommand.Queueable): Task[Unit] =
    dispatcher
      .enqueueCommand(cmd)
      .map(_
        .map(_.orThrow/*???*/)
        .onErrorHandle(t =>
          logger.error(s"${cmd.toShortString} => ${t.toStringWithCauses}",
            t.nullIfNoStackTrace))
        .startAndForget/* Don't await response */)

  private def postQueuedCommand(
    numberedCommand: Numbered[SubagentCommand.Queueable],
    subagentRunId: SubagentRunId,
    processingAllowed: Switch.ReadOnly)
  : Task[Checked[Unit]] =
    Task.defer {
      //val heartbeatTimeoutElapsed = scheduler.now + SubagentEventListener.heartbeatTiming.longHeartbeatTimeout
      val retryAfterError = new RetryAfterError(processingAllowed.whenOff)
      val command = numberedCommand.value
      lazy val commandString = numberedCommand.copy(value = command.toShortString).toString
      logger.traceTask("postQueuedCommand", commandString)(Task
        .tailRecM(())(_ =>
          // TODO Use processingAllowed when Order is being canceled
          currentSubagentItemState
            .flatMapT(subagentItemState => processingAllowed.isOff
              .flatMap(isStopped =>
                // Double-check subagentRunId to be sure.
                if (isStopped || !subagentItemState.subagentRunId.contains(subagentRunId)) {
                  logger.debug(s"postQueuedCommand($commandString) stopped")
                  Task.right(())
                } else
                  postQueuedCommand2(numberedCommand)))
            .materialize
            .flatMap {
              case Failure(throwable) =>
                retryAfterError(Problem.fromThrowable(throwable))

              case Success(checked @ Left(_: SubagentDriverStoppedProblem)) =>
                logger.debug(s"postQueuedCommand($commandString) stopped")
                Task.right(checked)

              case Success(checked @ Left(problem)) =>
                if (HttpClient.isTemporaryUnreachableStatus(problem.httpStatusCode))
                  // We don't check the error type,
                  // because it's seems impossible to properly detect recoverable errors.
                  // Instead we repeat the command until we are being stopped due
                  // to heartbeat timeout detection or other reason.
                  retryAfterError(problem)
                else
                  Task.right(checked)

              case Success(checked @ Right(_)) =>
                Task.right(checked)
            })
        .flatMap {
          case Left(SubagentNotDedicatedProblem) =>
            logger.debug(s"⚠️ $commandString => SubagentNotDedicatedProblem => ProcessLost")
            command match {
              case command: StartOrderProcess =>
                orderToPromise
                  .remove(command.orderId)
                  // Delay to let onSubagentDied go ahead and let it handle all orders at once
                  //?.delayExecution(100.ms)
                  .flatMap {
                    case None =>
                      // OrderProcessed has already been emitted by onSubagentDied)
                      // The command does not fail:
                      Task.right(())

                    case Some(promise) =>
                      // The SubagentEventListener should do the same for all lost processes
                      // at once, but just in case the SubagentEventListener does run, we issue
                      // an OrderProcess event here.
                      onStartOrderProcessFailed(command)
                        .map(_.map { _ =>
                          promise.success(OrderProcessed.processLost)
                          ()
                        })
                  }
              case _ =>
                Task.right((SubagentNotDedicatedProblem))
            }

          case Left(problem) =>
            processingAllowed.isOff.flatMap(if (_) {
              logger.debug(s"⚠️ postQueuedCommand($commandString) error after stop ignored: $problem")
              Task.right(())
            } else
              Task.left(problem))

          case Right(()) => Task.right(())
        })
    }

  private final class RetryAfterError(whenStopped: Task[Unit]) {
    private val startedAt = now
    private var lastWarning: Option[String] = None
    private var warningCount = 0

    def apply(problem: Problem): Task[Either[Unit, Right[Nothing, Unit]]] =
      Task
        .race(
          whenStopped.as(Right(Right(()))),
          retry(problem))
        .map(_.fold(identity, identity))

    private def retry(problem: Problem): Task[Left[Unit, Nothing]] =
      Task.defer {
        warningCount += 1
        val warning = problem.throwableOption match {
          case None => problem.toString
          case Some(t) => t.toStringWithCauses
        }
        if (lastWarning.contains(warning)) {
          logger.debug(s"⚠️ Retry warning #$warningCount (${startedAt.elapsed.pretty}) $warning")
        } else {
          lastWarning = Some(warning)
          logger.warn(s"⚠️ Retry warning #$warningCount $warning")
        }
        Task.sleep(tryPostErrorDelay) // Retry
          .as(Left(()))
      }
  }

  private def postQueuedCommand2(numberedCommand: Numbered[SubagentCommand.Queueable])
  : Task[Checked[Unit]] = {
    val command = numberedCommand.value
    dependentSignedItems(command)
      .map(_.orThrow)
      .flatMap { signedItems =>
        val cmd = signedItems match {
          case Nil => numberedCommand
          case signedSeq => numberedCommand.copy(
            value = SubagentCommand.Batch(signedSeq.map(AttachSignedItem(_)) :+ command))
        }
        HttpClient.liftProblem(
          client
            .executeSubagentCommand(cmd)
            .*>(attachedItemKeys.update(o => Task.pure(o ++ signedItems.view.map(_.value.keyAndRevision))))
            .void)
      }
  }

  private def dependentSignedItems(command: SubagentCommand): Task[Checked[Seq[Signed[SignableItem]]]] =
    command match {
      case startOrderProcess: StartOrderProcess =>
        val alreadyAttached = attachedItemKeys.get
        signableItemsForOrderProcessing(startOrderProcess.order.workflowPosition)
          .map(_.map(_.filterNot(signed =>
            alreadyAttached.get(signed.value.key) contains signed.value.itemRevision)))
      case _ =>
        Task.right(Nil)
    }

  private def signableItemsForOrderProcessing(workflowPosition: WorkflowPosition)
  : Task[Checked[Seq[Signed[SignableItem]]]] =
    for (agentState <- persistence.state) yield
      for {
        signedWorkflow <- agentState.keyToSigned(Workflow).checked(workflowPosition.workflowId)
        workflow = signedWorkflow.value
        jobKey <- workflow.positionToJobKey(workflowPosition.position)
        job <- workflow.keyToJob.checked(jobKey)
        jobResourcePaths = JobConf.jobResourcePathsFor(job, workflow)
        signedJobResources <- jobResourcePaths.traverse(agentState.keyToSigned(JobResource).checked)
      } yield signedJobResources :+ signedWorkflow

  private def currentSubagentItemState: Task[Checked[SubagentItemState]] =
    persistence.state.map(_.idToSubagentItemState.checked(subagentId))

  override def toString =
    s"RemoteSubagentDriver(${subagentItem.pathRev})"
}

object RemoteSubagentDriver
{
  private val reconnectErrorDelay = 5.s/*TODO*/
  private val tryPostErrorDelay = 5.s/*TODO*/
  private val CommandDispatcherStoppedProblem = Problem.pure("CommandDispatcher stopped")

  final case class SubagentDriverStoppedProblem(subagentId: SubagentId) extends Problem.Coded {
    def arguments = Map("subagentId" -> subagentId.string)
  }
}
