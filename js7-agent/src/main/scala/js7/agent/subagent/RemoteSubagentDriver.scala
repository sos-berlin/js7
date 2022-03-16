package js7.agent.subagent

import akka.actor.ActorSystem
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
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.item.{InventoryItemKey, ItemRevision, SignableItem}
import js7.data.job.{JobConf, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentRefStateEvent.{SubagentDedicated, SubagentDied, SubagentRestarted}
import js7.data.subagent.{SubagentId, SubagentRef, SubagentRefState, SubagentRunId}
import js7.data.workflow.Workflow
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.StatePersistence
import js7.subagent.client.{SubagentClient, SubagentDriver}
import js7.subagent.configuration.SubagentConf
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachSignedItem, CoupleDirector, DedicateSubagent, KillProcess, StartOrderProcess}
import monix.eval.Task
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.util.{Failure, Success}

final class RemoteSubagentDriver(
  val subagentRef: SubagentRef,
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

  private val logger = Logger.withPrefix[this.type](subagentId.toString)
  private val dispatcher = new SubagentDispatcher(subagentId, postQueuedCommand)
  private val attachedItemKeys = AsyncVariable(Map.empty[InventoryItemKey, Option[ItemRevision]])
  @volatile private var stopping = false
  @volatile private var shuttingDown = false

  def subagentId = subagentRef.id

  def isStopping = stopping

  def isShuttingDown = shuttingDown

  protected val client = new SubagentClient(
    Admission(
      subagentRef.uri,
      subagentConf.config
        .optionAs[SecretString](
          "js7.auth.subagents." + ConfigUtil.joinPath(subagentRef.id.string))
        .map(UserAndPassword(subagentRef.agentPath.toUserId.orThrow, _))),
    httpsConfig,
    name = subagentRef.id.toString,
    actorSystem)

  private val orderToProcessing = new AsyncMap[OrderId, Promise[OrderProcessed]]
    with AsyncMap.Stoppable

  def start: Task[Unit] =
    logger
      .debugTask(
        dedicateOrCouple
          .map(_.orThrow)
          .flatMap { case (subagentRunId, _) =>
            startEventListener *> dispatcher.start(subagentRunId)
          })
      .memoize

  def stop(ignoredSignal: Option[ProcessSignal]): Task[Unit] =
    logger.debugTask(Task.defer {
      stopping = true
      Task.parZip2(dispatcher.stop, stopEventListener)
        .*>(client.tryLogout.void)
        .logWhenItTakesLonger(s"RemoteSubagentDriver($subagentId).stop")
    })

  def shutdown: Task[Unit] =
    logger.debugTask(Task.defer {
      shuttingDown = true
      orderToProcessing.stop
        // Emit event and change state ???
        .*>(tryShutdownSubagent)
    })

  private def tryShutdownSubagent: Task[Unit] =
    client
      .login(onlyIfNotLoggedIn = true)
      .*>(client.executeSubagentCommand(Numbered(0, SubagentCommand.ShutDown(restart = true))))
      .void
      .onErrorHandle(t =>  // Ignore when Subagent is unreachable
        logger.error(s"SubagentCommand.ShutDown => ${t.toStringWithCauses}"))

  protected def dedicateOrCouple: Task[Checked[(SubagentRunId, EventId)]] =
    logger.debugTask(
      currentSubagentRefState
        .flatMapT(subagentRefState =>
          dedicateOrCouple2(subagentRefState).map(Right(_))))

  private def dedicateOrCouple2(subagentRefState: SubagentRefState): Task[(SubagentRunId, EventId)] =
    subagentRefState.subagentRunId match {
      case None =>
        for {
          response <- dedicate
          eventId <- couple(response.subagentRunId, response.subagentEventId)
        } yield (response.subagentRunId, eventId)

      case Some(subagentRunId) =>
        couple(subagentRunId, subagentRefState.eventId)
          .map(subagentRunId -> _)
    }

  private def dedicate: Task[DedicateSubagent.Response] = {
    val cmd = DedicateSubagent(subagentId, subagentRef.agentPath, controllerId)
    postCommandUntilSucceeded(cmd)
      .flatMap(response => persistence
        .persistKeyedEvent(subagentId <-: SubagentDedicated(response.subagentRunId))
        .rightAs(response)
        .map(_.orThrow)
        .<*(dispatcher.start(response.subagentRunId)))
  }

  private def couple(subagentRunId: SubagentRunId, eventId: EventId): Task[EventId] =
    Task.defer {
      val cmd = CoupleDirector(subagentId, subagentRunId, eventId,
        SubagentEventListener.heartbeatTiming)
      Task.tailRecM(())(_ =>
        // TODO Must be stoppable
        postCommand(Numbered(0, cmd))
          .as(Right(eventId))
          .onErrorRecoverWith {
            case HttpException.HasProblem(SubagentNotDedicatedProblem) =>
              orderToProcessing.toMap.size match {
                case 0 => logger.info("Subagent restarted")
                case n => logger.warn(s"Subagent restarted and lost $n Order processes")
              }
              onSubagentDied(SubagentRestarted)
                .*>(dedicate)
                .map(response => Right(response.subagentEventId))

            case throwable =>
              logger.warn(s"DedicateSubagent failed: ${throwable.toStringWithCauses}")
              Task.left(()).delayExecution(reconnectErrorDelay)
          })
    }

  // May run concurrently with onStartOrderProcessFailed !!!
  // Be sure that only on OrderProcessed event is emitted!
  /** Emit OrderProcessed(ProcessLost) and `subagentDied` events. */
  protected def onSubagentDied(subagentDied: SubagentDied): Task[Unit] = {
    // Subagent died and lost its state
    // Emit OrderProcessed(Disrupted(ProcessLost)) for each processing order.
    // Then subagentDied
    val processing = Order.Processing(subagentId)
    orderToProcessing.removeAll
      .flatMap { oToP =>
        val orderIds = oToP.keys
        val promises = oToP.values
        dispatcher
          .stop
          .*>(attachedItemKeys.update(_ => Task.pure(Map.empty)))
          .*>(persistence
              .persist(state => Right(orderIds.view
                .filter(orderId =>
                  // Just to be sure, condition should always be true:
                  state.idToOrder.get(orderId).exists(_.state == processing))
                .map(_ <-: OrderProcessed.processLost)
                .concat((subagentId <-: subagentDied) :: Nil)
                .toVector))
              .map(_.orThrow)
              .*>(Task {
                for (p <- promises) p.success(OrderProcessed.processLost)
              }))
      }
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

  // TODO Call this to update a changed JobResource
  //private def onSignedItemChanged(signedItem: Signed[SignableItem]): Task[Unit] =
  //  Task.defer {
  //    if (stopping)
  //      Task.unit
  //    else
  //      executeCommands(AttachSignedItem(signedItem) :: Nil)
  //  }

  def processOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]] =
    logger.traceTask("processOrder", order.id)(
      orderToExecuteDefaultArguments(order)
        .flatMapT(defaultArguments =>
          ifNotStopping.flatMapT(_ =>
            runProcessingOrder(order)(
              dispatcher
                .executeCommand(StartOrderProcess(order, defaultArguments))
                .rightAs(())))))

  def continueProcessingOrder(order: Order[Order.Processing]) =
    processOrder(order)

  private def runProcessingOrder(order: Order[Order.Processing])(body: Task[Checked[Unit]])
  : Task[Checked[OrderProcessed]] =
    orderToProcessing
      .insert(order.id, Promise())
      // OrderProcessed event will fulfill the promise and remove the Processing entry
      .flatMapT(promisedOutcome =>
        body
          .flatMap {
            case Left(problem) =>
              orderToProcessing
                .remove(order.id)
                .flatMap {
                  case None => Task.left(problem)
                  case Some(_) =>
                    val orderProcessed = OrderProcessed(Outcome.Disrupted(problem))
                    persistence
                      .persistKeyedEvent(order.id <-: orderProcessed)
                      .rightAs(orderProcessed)
                }
            case Right(()) =>
              Task.fromFuture(promisedOutcome.future)
                .map(Right(_))
          })

  protected def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed)
  : Task[Option[Task[Unit]]] =
    orderToProcessing.remove(orderId).map {
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")
        None

      case Some(processing) =>
        Some(Task(processing.success(orderProcessed)))
    }

  private def killAll(signal: ProcessSignal): Task[Unit] =
    Task.defer {
      val cmds = orderToProcessing.toMap.keys.map(KillProcess(_, signal))
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
          currentSubagentRefState
            .flatMapT(subagentRefState => processingAllowed.isOff
              .flatMap(isStopped =>
                // Double-check subagentRunId to be sure.
                if (isStopped || !subagentRefState.subagentRunId.contains(subagentRunId)) {
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
            logger.debug(
              s"$commandString => SubagentNotDedicatedProblem => ProcessLost")
            command match {
              case command: StartOrderProcess =>
                orderToProcessing
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
              logger.debug(s"postQueuedCommand($commandString) error after stop ignored: $problem")
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
          logger.debug(s"#$warningCount (${startedAt.elapsed.pretty}) $warning")
        } else {
          lastWarning = Some(warning)
          logger.warn(s"#$warningCount $warning")
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

  //private def dependentCommands(command: SubagentCommand): Task[Checked[Seq[SubagentCommand]]] =
  //  command match {
  //    case startOrderProcess: StartOrderProcess =>
  //      signableItemsForOrderProcessing(startOrderProcess.order.workflowPosition)
  //        .map(_.map(_
  //          .filterNot(signed => attachedItemKeys(signed.value.key))
  //          .map(AttachSignedItem(_))))
  //    case _ =>
  //      Task.right(Nil)
  //  }

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

  private def currentSubagentRefState: Task[Checked[SubagentRefState]] =
    persistence.state.map(_.idToSubagentRefState.checked(subagentId))

  override def toString =
    s"RemoteSubagentDriver(${subagentId.string})"
}

object RemoteSubagentDriver
{
  private val reconnectErrorDelay = 5.s/*TODO*/
  private val tryPostErrorDelay = 5.s/*TODO*/

  final case class SubagentDriverStoppedProblem(subagentId: SubagentId) extends Problem.Coded {
    def arguments = Map("subagentId" -> subagentId.string)
  }
}
