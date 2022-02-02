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
import js7.base.web.HttpClient.HttpException
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.item.InventoryItem
import js7.data.job.{JobConf, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.Outcome.Disrupted.JobSchedulerRestarted
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentRefStateEvent.{SubagentDedicated, SubagentReset}
import js7.data.subagent.{SubagentRef, SubagentRefState, SubagentRefStateEvent}
import js7.data.value.expression.Expression
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.StatePersistence
import js7.subagent.client.{SubagentClient, SubagentDriver}
import js7.subagent.data.SubagentCommand
import js7.subagent.data.SubagentCommand.{CoupleDirector, DedicateSubagent, KillProcess}
import monix.eval.Task
import scala.concurrent.Promise

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
  private val commandQueue = new CommandQueue(subagentId, postCommand)

  protected val client = new SubagentClient(
    Admission(subagentRef.uri, userAndPassword),
    httpsConfig,
    name = subagentRef.id.toString,
    actorSystem)

  private val orderToProcessing = new AsyncMap[OrderId, Promise[OrderProcessed]]
    with AsyncMap.Stoppable

  @volatile private var stopping = false

  def start: Task[Unit] =
    logger.debugTask(
      dedicateOrCouple
        .map(_.orThrow)
        .*>(startEventListener)
        .*>(commandQueue.start)
        .memoize)

  def stop(ignoredSignal: Option[ProcessSignal]): Task[Unit] =
    logger.debugTask(Task.defer {
      stopping = true
      Task.parZip2(commandQueue.stop, stopEventListener)
        .*>(client.tryLogout.void)
        .logWhenItTakesLonger(s"RemoteSubagentDriver($subagentId).stop")
    })

  protected def dedicateOrCouple: Task[Checked[EventId]] =
    logger.debugTask(
      subagentRefState
        .flatMapT(subagentRefState =>
          dedicateOrCouple2(subagentRefState).map(Right(_))))

  private def dedicateOrCouple2(subagentRefState: SubagentRefState): Task[EventId] =
    subagentRefState.subagentRunId match {
      case None =>
        dedicate.as(EventId.BeforeFirst)

      case Some(subagentRunId) =>
        val cmd = CoupleDirector(subagentId, subagentRunId, subagentRefState.eventId)
        postCommand(Numbered(0, cmd))
          .as(subagentRefState.eventId)
          .onErrorRestartLoop(()) {
            case (t @ HttpException.HasProblem(SubagentNotDedicatedProblem), _, _) =>
              Task.raiseError(t)

            case (throwable, _, retry) =>
              logger.error("DedicateSubagent failed: " + throwable.toStringWithCauses)
              retry(()).delayExecution(reconnectAfterErrorDelay)
          }
          .onErrorRecoverWith {
            case HttpException.HasProblem(SubagentNotDedicatedProblem) =>
              if (orderToProcessing.toMap.nonEmpty) {
                logger.warn("Subagent restarted and lost its Order processes")
              }
              //commandQueue.stop/*???*/ *>
              onSubagentLost(SubagentReset)
                .*>(dedicate)
                .as(EventId.BeforeFirst)
          }
    }

  def onSubagentLost(subagentLostEvent: SubagentRefStateEvent): Task[Unit] = {
    // Subagent has restarted and lost its state
    // Emit OrderProcessed(Disrupted(JobSchedulerRestarted)) for all processing orders.
    // Then SubagentReset
    val processed = OrderProcessed(Outcome.Disrupted(JobSchedulerRestarted))
    val processing = Order.Processing(subagentId)
    orderToProcessing.removeAll
      .flatMap { oToP =>
        val (orderIds, promises) = oToP.view.unzip
        for (o <- orderIds) logger.warn(s"Lost process for $o")
        persistence
          .persist(state => Right(orderIds
            .filter(orderId =>
              // Just to be sure, should always be true:
              state.idToOrder.get(orderId).exists(_.state == processing))
            .map(_ <-: processed)
            .concat((subagentId <-: subagentLostEvent) :: Nil)
            .toVector))
          .map(_.orThrow)
          .*>(Task {
            for (p <- promises) p.success(processed)
          })
      }
    }

  private def dedicate: Task[Unit] =
    postCommandUntilSucceeded(DedicateSubagent(subagentId, subagentRef.agentPath, controllerId))
      .flatMap(response => persistence
        .persistKeyedEvent(subagentId <-: SubagentDedicated(response.subagentRunId)))
      .map(_.orThrow)

  // TODO Attach only new items
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
    ifNotStopping.flatMapT(_ =>
      runProcessingOrder(order)(
        startProcess(order, defaultArguments)))

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

  private def startProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Unit]] =
    itemsForOrderProcessing(order.workflowPosition)
      .flatMapT(commandQueue.startProcess(order, defaultArguments, _))

  protected def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed): Task[Unit] =
    orderToProcessing.remove(orderId).map {
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")

      case Some(processing) =>
        processing.success(orderProcessed)
    }

  private def killAll(signal: ProcessSignal): Task[Unit] =
    commandQueue
      .executeCommands(
        orderToProcessing.toMap.view
          .filterNot(_._2.isCompleted)
          .keys
          .map(KillProcess(_, signal)))
      .map {
        case Left(problem) => logger.error(s"killAll => $problem")
        case Right(()) =>
      }

  def killProcess(orderId: OrderId, signal: ProcessSignal) =
    commandQueue.executeCommand(KillProcess(orderId, signal))
      .map {
        // Subagent may have been restarted
        case Left(problem) => logger.error(s"killProcess $orderId => $problem")
        case Right(()) =>
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
        logger.error(command.getClass.simpleScalaName + " failed: " + throwable.toStringWithCauses)
        retry(()).delayExecution(5.s/*TODO*/)
      }

  private def postCommand(command: Numbered[SubagentCommand]): Task[SubagentCommand.Response] =
    client.retryUntilReachable()(
      client.loginUntilReachable(onlyIfNotLoggedIn = true) *>
        client.executeSubagentCommand(command)
    ).onErrorRestartLoop(()) { (throwable, _, retry) =>
      val msg = throwable.getMessage
      if (msg.contains("TCP") && msg.contains("Connection reset by peer")) // ???
        retry(()).delayExecution(5.s/*TODO*/) // TODO Must be stoppable, but no OrderProcessed
      else
        Task.raiseError(throwable)
    }

  private def subagentRefState: Task[Checked[SubagentRefState]] =
    persistence.state.map(_.idToSubagentRefState.checked(subagentId))

  override def toString =
    s"RemoteSubagentDriver(${subagentId.string})"
}

object RemoteSubagentDriver
{
  private val reconnectAfterErrorDelay = 5.s/*TODO*/
}
