package js7.subagent

import cats.effect.ExitCase
import js7.base.Js7Version
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{ProgramTermination, SetOnce}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.Problems.{SubagentAlreadyDedicatedProblem, SubagentIdMismatchProblem, SubagentIsShuttingDownProblem, SubagentRunIdMismatchProblem}
import js7.data.subagent.SubagentCommand.{CoupleDirector, DedicateSubagent, ShutDown}
import js7.data.subagent.SubagentEvent.SubagentShutdown
import js7.data.subagent.{SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.data.workflow.position.WorkflowPosition
import js7.journal.watch.InMemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.SubagentExecutor.*
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import monix.execution.atomic.Atomic

trait SubagentExecutor
{
  protected[this] val dedicatedOnce: SetOnce[Dedicated]
  protected val journal: InMemoryJournal[SubagentState]
  protected val subagentConf: SubagentConf
  protected val jobLauncherConf: JobLauncherConf

  protected def onStop = Task.unit

  val subagentRunId = SubagentRunId.fromJournalId(journal.journalId)
  private val subagentDriverConf = SubagentDriver.Conf.fromConfig(subagentConf.config,
    commitDelay = 0.s)
  private val orderToProcessing = AsyncMap.stoppable[OrderId, Processing]()

  private val shuttingDown = Atomic(false)
  private val terminatedOnce = SetOnce[ProgramTermination]
  @volatile private var _dontWaitForDirector = false

  def isShuttingDown: Boolean =
    shuttingDown()

  def untilTerminated: Task[ProgramTermination] =
    terminatedOnce.task

  final def shutdown(shutDown: ShutDown): Task[ProgramTermination] = {
    import shutDown.{dontWaitForDirector, processSignal, restart}
    logger.debugTask(
      "shutdown",
      Seq(processSignal, restart ? "restart", dontWaitForDirector ? "dontWaitForDirector")
        .flatten.mkString(" ")
    )(Task.defer {
      _dontWaitForDirector |= dontWaitForDirector
      val first = !shuttingDown.getAndSet(true)
      Task
        .when(first)(
          onStop.*>(dedicatedOnce
            .toOption
            .fold(Task.unit)(_.localSubagentDriver.stop(processSignal))
            .*>(orderToProcessing.initiateStopWithProblem(SubagentIsShuttingDownProblem))
            .*>(Task.defer {
              val orderIds = orderToProcessing.toMap.keys.toVector.sorted
              if (dontWaitForDirector) Task {
                for (orderId <- orderIds) logger.warn(
                  s"$shutDown: Agent Director has not yet acknowledged processing of $orderId")
              } else Task.defer {
                for (orderId <- orderIds) logger.info(
                  s"🟡 Delaying $shutDown until Agent Director has acknowledged processing of $orderId")
                  // Await process termination and DetachProcessedOrder commands
                orderToProcessing.whenStopped
                  .logWhenItTakesLonger("Director-acknowledged Order processes")
              }
            })
            .*>(journal
              // The event may get lost due to immediate shutdown !!!
              .persistKeyedEvent(NoKey <-: SubagentShutdown)
              .rightAs(())
              .map(_.onProblemHandle(problem => logger.warn(s"Shutdown: $problem"))))
            .*>(Task {
              logger.info(
                s"Subagent${dedicatedOnce.toOption.fold("")(_.toString + " ")} stopped")
              terminatedOnce.trySet(ProgramTermination(restart = restart))
            })))
        .*>(terminatedOnce.task)
    })
  }

  protected def executeDedicateSubagent(cmd: DedicateSubagent)
  : Task[Checked[DedicateSubagent.Response]] =
    Task.defer {
      val isFirst = dedicatedOnce.trySet(
        new Dedicated(
          newLocalSubagentDriver(cmd.subagentId, cmd.agentPath, cmd.controllerId)))
      if (!isFirst) {
        // TODO Idempotent: Frisch gewidmeter Subagent ist okay. Kein Kommando darf eingekommen sein.
        //if (cmd.subagentId == dedicatedOnce.orThrow.subagentId)
        //  Task.pure(Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst)))
        //else
        logger.warn(s"$cmd => $SubagentAlreadyDedicatedProblem: $dedicatedOnce")
        Task.left(SubagentAlreadyDedicatedProblem)
      } else {
        // TODO Check agentPath, controllerId (handle in SubagentState?)
        logger.info(s"Subagent dedicated to be ${cmd.subagentId} in ${cmd.agentPath}, is ready")
        Task.right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst, Some(Js7Version)))
      }
    }

  private def newLocalSubagentDriver(
    subagentId: SubagentId,
    agentPath: AgentPath,
    controllerId: ControllerId)
  =
    new LocalSubagentDriver[SubagentState](
      subagentId,
      journal,
      agentPath,
      controllerId,
      jobLauncherConf,
      subagentDriverConf,
      subagentConf)

  protected def executeCoupleDirector(cmd: CoupleDirector): Task[Checked[Unit]] =
    Task {
      for {
        _ <- checkSubagentId(cmd.subagentId)
        _ <- checkSubagentRunId(cmd.subagentRunId)
        _ <- journal.checkEventId(cmd.eventId)
      } yield ()
    }

  private def checkSubagentId(requestedSubagentId: SubagentId): Checked[Unit] =
    dedicatedOnce.checked.flatMap(dedicated =>
      if (requestedSubagentId != dedicated.subagentId)
        Left(SubagentIdMismatchProblem(requestedSubagentId, dedicated.subagentId))
      else
        RightUnit)

  def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
    dedicatedOnce.checked.flatMap(dedicated =>
      if (requestedSubagentRunId != this.subagentRunId) {
        val problem = SubagentRunIdMismatchProblem(dedicated.subagentId)
        logger.warn(
          s"$problem, requestedSubagentRunId=$requestedSubagentRunId, " +
            s"agentRunId=${this.subagentRunId}")
        Left(problem)
      } else
        Checked.unit)

  protected final def startOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Unit]] =
    Task.defer {
      orderToProcessing
        .updateChecked(order.id, {
          case Some(existing) =>
            Task.pure(
              if (existing.workflowPosition != order.workflowPosition) {
                val problem = Problem.pure("Duplicate SubagentCommand.StartOrder with different Order position")
                logger.warn(s"$problem:")
                logger.warn(s"  Added order   : ${order.id} ${order.workflowPosition}")
                logger.warn(s"  Existing order: ${order.id} ${existing.workflowPosition}")
                Left(problem)
              } else
                Right(existing)) // Idempotency: Order process has already been started

          case None =>
            Task(dedicatedOnce.checked).flatMapT(dedicated =>
              processOrder(dedicated.localSubagentDriver, order, defaultArguments)
                .guaranteeCase {
                  case ExitCase.Completed =>
                    Task.unless(!_dontWaitForDirector) {
                      logger.warn(
                        s"dontWaitForDirector: ${order.id} <-: OrderProcessed event may get lost")
                      orderToProcessing.remove(order.id).void
                    }

                  case _ => orderToProcessing.remove(order.id).void // Tidy-up on failure
                }
                .startAndForget
                // TODO Asynchronous processOrder should not execute AFTER shutdown
                .as(Right(Processing(order.workflowPosition))))
        })
        .rightAs(())
    }

  private def processOrder(
    driver: LocalSubagentDriver[SubagentState],
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[EventId] =
    driver
      .processOrder2(order, defaultArguments)
      .onErrorHandle(Outcome.Failed.fromThrowable)
      .flatMap { outcome =>
        val orderProcessed = order.id <-: OrderProcessed(outcome)
        journal
          .persistKeyedEvent(orderProcessed)
          .map(_.orThrow._1.eventId)
      }

  protected def detachProcessedOrder(orderId: OrderId): Task[Checked[Unit]] =
    orderToProcessing.remove(orderId).as(Checked.unit)

  protected def releaseEvents(eventId: EventId): Task[Checked[Unit]] =
    journal.releaseEvents(eventId)

  def subagentId: Option[SubagentId] =
    dedicatedOnce.toOption.map(_.subagentId)
}

object SubagentExecutor
{
  private val logger = Logger(getClass)

  private[subagent] final class Dedicated(
    val localSubagentDriver: LocalSubagentDriver[SubagentState])
  {
    def subagentId = localSubagentDriver.subagentId
    def agentPath = localSubagentDriver.agentPath
    def controllerId = localSubagentDriver.controllerId

    override def toString = s"Dedicated($subagentId $agentPath $controllerId)"
  }

  private final case class Processing(
    workflowPosition: WorkflowPosition/*for check only*/)
}
