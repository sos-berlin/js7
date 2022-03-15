package js7.subagent

import cats.effect.ExitCase
import js7.agent.data.Problems.{SubagentIdMismatchProblem, SubagentRunIdMismatchProblem}
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{Base64UUID, ProgramTermination, SetOnce}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.data.value.expression.Expression
import js7.data.workflow.position.WorkflowPosition
import js7.journal.watch.InMemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.SubagentExecutor._
import js7.subagent.client.SubagentDriver
import js7.subagent.configuration.SubagentConf
import js7.subagent.data.SubagentCommand.{CoupleDirector, DedicateSubagent}
import js7.subagent.data.SubagentEvent.{SubagentDedicated, SubagentShutdown}
import monix.eval.Task
import monix.execution.atomic.Atomic

trait SubagentExecutor
{
  protected[this] val dedicatedOnce: SetOnce[Dedicated]
  protected val journal: InMemoryJournal[SubagentState]  // FIXME Limit number of events!
  protected val subagentConf: SubagentConf
  protected val jobLauncherConf: JobLauncherConf

  private val subagentDriverConf = SubagentDriver.Conf.fromConfig(subagentConf.config,
    commitDelay = 0.s)
  private val orderToProcessing = new AsyncMap(Map.empty[OrderId, Processing])
    with AsyncMap.Stoppable
  private val subagentRunId = SubagentRunId(Base64UUID.random())

  private val shutdownStarted = Atomic(false)
  private val stoppedOnce = SetOnce[ProgramTermination]
  @volatile private var dontWaitForDirector = false

  def untilStopped: Task[ProgramTermination] =
    stoppedOnce.task

  final def shutdown(
    signal: Option[ProcessSignal] = None,
    restart: Boolean = false,
    dontWaitForDirector: Boolean = false)
  : Task[ProgramTermination] =
    logger.debugTask(
      "shutdown",
      s"${signal getOrElse ""}${restart ?? " restart"}${dontWaitForDirector ?? " dontWaitForDirector"}"
    )(Task.defer {
      this.dontWaitForDirector |= dontWaitForDirector
      val first = !shutdownStarted.getAndSet(true)
      Task
        .when(first)(
          dedicatedOnce
            .toOption
            .fold(Task.unit)(_.subagentDriver.stop(signal))
            .*>(orderToProcessing
              // Await process termination and DetachProcessedOrder commands
              .stop
              .logWhenItTakesLonger("Director-acknowledged Order processes"))
          .*>(journal
            // The event probably gets lost due to immediate shutdown !!!
            .persistKeyedEvent(NoKey <-: SubagentShutdown)
            .map(_.onProblemHandle(problem => logger.warn(s"Shutdown: $problem"))))
          .*>(Task {
            logger.info(
              s"Subagent${dedicatedOnce.toOption.fold("")(_.toString + " ")} stopped")
            val t = ProgramTermination(restart = restart)
            stoppedOnce.trySet(t)
            }))
        .*>(stoppedOnce.task)
    })

  protected def executeDedicateSubagent(cmd: DedicateSubagent)
  : Task[Checked[DedicateSubagent.Response]] =
    Task.defer {
      val isFirst = dedicatedOnce.trySet(
        new Dedicated(
          newLocalSubagentDriver(cmd.subagentId, cmd.agentPath, cmd.controllerId)))
      if (!isFirst) {
        // TODO Idempotent: Frisch gewidmeter Subagent ist okay. Kein Kommando darf eingekommen sein.
        //if (cmd.subagentId == dedicatedOnce.orThrow.subagentId)
        //  Task.pure(Rightm(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst)))
        //else
        Task.pure(Left(Problem.pure(
          s"This Subagent has already been dedicated: $dedicatedOnce")))
      } else {
        val event = SubagentDedicated(cmd.subagentId, subagentRunId, cmd.agentPath, cmd.controllerId)
        // TODO Check agentPath, controllerId (handle in SubagentState?)
        journal.persistKeyedEvent(NoKey <-: event)
          .map(_.map(_ =>
            logger.info(s"Subagent dedicated to be ${cmd.subagentId} in ${cmd.agentPath}, is ready")))
          .rightAs(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst))
      }
    }

  private def newLocalSubagentDriver(
    subagentId: SubagentId,
    agentPath: AgentPath,
    controllerId: ControllerId)
  =
    new LocalSubagentDriver(
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

  private def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
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
    orderToProcessing
      .updateChecked(order.id, {
        case Some(existing) =>
          Task.pure(
            if (existing.workflowPosition != order.workflowPosition)
              Left(Problem.pure("Duplicate SubagentCommand.StartOrder with different Order"))
            else
              Right(existing)) // Idempotency: Order process has already been started

        case None =>
          Task(dedicatedOnce.checked).flatMapT(dedicated =>
            processOrder(dedicated.subagentDriver, order, defaultArguments)
              .guaranteeCase {
                case ExitCase.Completed =>
                  if (!dontWaitForDirector)
                    Task.unit
                  else {
                    logger.warn(
                      s"dontWaitForDirector: ${order.id} <-: OrderProcessed")
                    orderToProcessing.remove(order.id).void
                  }

                case _ => orderToProcessing.remove(order.id).void // Tidy-up on failure
              }
              .startAndForget
              .as(Right(Processing(order.workflowPosition))))
      })
      .rightAs(())

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
}

object SubagentExecutor
{
  private val logger = Logger(getClass)

  private[subagent] final class Dedicated(
    val subagentDriver: LocalSubagentDriver[SubagentState])
  {
    def subagentId = subagentDriver.subagentId
    def agentPath = subagentDriver.agentPath
    def controllerId = subagentDriver.controllerId

    override def toString = s"Dedicated($subagentId $agentPath $controllerId)"
  }

  private final case class Processing(
    workflowPosition: WorkflowPosition/*for check only*/)
}
