package js7.agent.subagent

import akka.actor.ActorSystem
import cats.syntax.traverse._
import js7.agent.data.AgentState
import js7.agent.data.Problems.SubagentNotDedicatedProblem
import js7.agent.subagent.RemoteSubagentDriver._
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.io.https.HttpsConfig
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax._
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
import js7.data.item.InventoryItem
import js7.data.job.{JobConf, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.SubagentRefStateEvent.{SubagentDedicated, SubagentReset}
import js7.data.subagent.{SubagentId, SubagentRef, SubagentRefState, SubagentRefStateEvent, SubagentRunId}
import js7.data.value.expression.Expression
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.StatePersistence
import js7.subagent.client.{SubagentClient, SubagentDriver}
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{AttachItem, CoupleDirector, DedicateSubagent, KillProcess, StartOrderProcess}
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.util.{Failure, Success}

final class RemoteSubagentDriver(
  val subagentRef: SubagentRef,
  userAndPassword: Option[UserAndPassword],
  httpsConfig: HttpsConfig,
  protected val persistence: StatePersistence[AgentState],
  controllerId: ControllerId,
  protected val conf: SubagentDriver.Conf,
  protected val recouplingStreamReaderConf: RecouplingStreamReaderConf,
  actorSystem: ActorSystem)
extends SubagentDriver with SubagentEventListener
{
  protected type S = AgentState

  def subagentId = subagentRef.id

  private val logger = Logger.withPrefix[this.type](subagentId.toString)
  private val dispatcher = new SubagentDispatcher(subagentId, postQueuedCommand)
  private val isDedicatingOrCoupling = Atomic(false)

  protected val client = new SubagentClient(
    Admission(subagentRef.uri, userAndPassword),
    httpsConfig,
    name = subagentRef.id.toString,
    actorSystem)

  private val orderToProcessing = new AsyncMap[OrderId, Promise[OrderProcessed]]
    with AsyncMap.Stoppable

  @volatile private var stopping = false

  // TODO Inhibit duplicate start (like SubagentEventListener)
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
        // TODO Duplicate with SubagentEventListener SubagentDedicated ?
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
              onSubagentDied(SubagentReset)
                .*>(dedicate)
                .map(response => Right(response.subagentEventId))

            case throwable =>
              logger.warn(s"DedicateSubagent failed: ${throwable.toStringWithCauses}")
              Task.left(()).delayExecution(reconnectErrorDelay)
          })
    }

  // May run concurrently with onStartOrderProcessDied !!!
  // Be sure that only on OrderProcessed event is emitted!
  /** Emit OrderProcessed(ProcessLost) and an `subagentLostEvent`. */
  protected def onSubagentDied(subagentLostEvent: SubagentRefStateEvent): Task[Unit] = {
    // Subagent died and lost its state
    // Emit OrderProcessed(Disrupted(ProcessLost)) for each processing order.
    // Then SubagentReset
    val processing = Order.Processing(subagentId)
    orderToProcessing.removeAll
      .flatMap { oToP =>
        val orderIds = oToP.keys
        val promises = oToP.values
        dispatcher
          .stop
          .*>(persistence
              .persist(state => Right(orderIds.view
                .filter(orderId =>
                  // Just to be sure, condition should always be true:
                  state.idToOrder.get(orderId).exists(_.state == processing))
                .map(_ <-: OrderProcessed.processLost)
                .concat((subagentId <-: subagentLostEvent) :: Nil)
                .toVector))
              .map(_.orThrow)
              .*>(Task {
                for (p <- promises) p.success(OrderProcessed.processLost)
              }))
      }
  }

  // May run concurrently with onSubagentDied !!!
  // Be sure that only on OrderProcessed event is emitted!
  private def onStartOrderProcessDied(startOrderProcess: StartOrderProcess): Task[Checked[Unit]] =
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

  def processOrder(order: Order[Order.Processing], defaultArguments: Map[String, Expression])
  : Task[Checked[OrderProcessed]] =
    logger.traceTask("processOrder", order.id)(
      ifNotStopping.flatMapT(_ =>
        runProcessingOrder(order)(
          dispatcher
            .executeCommand(StartOrderProcess(order, defaultArguments))
            .rightAs(()))))

  def continueProcessingOrder(order: Order[Order.Processing]) =
    runProcessingOrder(order)(Task.right(()))

  private def runProcessingOrder(order: Order[Order.Processing])(body: Task[Checked[Unit]])
  : Task[Checked[OrderProcessed]] =
    orderToProcessing
      .insert(order.id, Promise())
      // OrderProcessed event will fulfill the promise and remove the Processing entry
      .flatMapT(promisedOutcome =>
        body
          .flatMapT(_ => Task
            .fromFuture(promisedOutcome.future)
            .map(Right(_))))

  protected def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed): Task[Unit] =
    orderToProcessing.remove(orderId).map {
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")

      case Some(processing) =>
        processing.success(orderProcessed)
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

  private def postQueuedCommand(
    numberedCommand: Numbered[SubagentCommand.OrderCommand],
    subagentRunId: SubagentRunId,
    isStopped: Task[Boolean])
  : Task[Checked[Unit]] =
    Task.defer {
      //val heartbeatTimeoutElapsed = scheduler.now + SubagentEventListener.heartbeatTiming.longHeartbeatTimeout
      val retryAfterError = new RetryAfterError
      val command = numberedCommand.value
      lazy val commandString = numberedCommand.copy(value = command.toShortString).toString
      logger.traceTask("postQueuedCommand", commandString)(Task
        .tailRecM(())(_ =>
          // TODO Use isStopped when Order is being canceled
          currentSubagentRefState
            .flatMapT(subagentRefState => isStopped
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
                      onStartOrderProcessDied(command)
                        .map(_.map { _ =>
                          promise.success(OrderProcessed.processLost)
                          ()
                        })
                  }
              case _ =>
                Task.right((SubagentNotDedicatedProblem))
            }

          case Left(problem) =>
            isStopped.flatMap(if (_) {
              logger.debug(s"postQueuedCommand($commandString) error after stop ignored: $problem")
              Task.right(())
            } else
              Task.left(problem))

          case Right(()) => Task.right(())
        })
    }

  private final class RetryAfterError {
    private val startedAt = now
    private var lastWarning: Option[String] = None
    private var warningCount = 0

    def apply(problem: Problem): Task[Left[Unit, Nothing]] =
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

  private def postQueuedCommand2(numberedCommand: Numbered[SubagentCommand.OrderCommand])
  : Task[Checked[Unit]] = {
    val command = numberedCommand.value
    dependentCommands(command)
      .map(_.orThrow)
      .map {
        case Nil => numberedCommand
        case depCmds => numberedCommand.copy(
          value = SubagentCommand.Batch(depCmds :+ command))
      }
      .flatMap(cmd => HttpClient.liftProblem(
        client.executeSubagentCommand(cmd).void))
  }

  private def dependentCommands(command: SubagentCommand): Task[Checked[Seq[SubagentCommand]]] =
    command match {
      case startOrderProcess: StartOrderProcess =>
        // TODO Attach only new items
        itemsForOrderProcessing(startOrderProcess.order.workflowPosition)
          .map(_.map(_.map(AttachItem(_))))
      case _ =>
        Task.right(Nil)
    }

  private def itemsForOrderProcessing(workflowPosition: WorkflowPosition)
  : Task[Checked[Seq[InventoryItem]]] =
    for (agentState <- persistence.state) yield
      for {
        workflow <- agentState.idToWorkflow.checked(workflowPosition.workflowId)
        jobKey <- workflow.positionToJobKey(workflowPosition.position)
        job <- workflow.keyToJob.checked(jobKey)
        jobResourcePaths = JobConf.jobResourcePathsFor(job, workflow)
        jobResources <- jobResourcePaths.traverse(agentState.keyTo(JobResource).checked)
      } yield jobResources :+ workflow

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
