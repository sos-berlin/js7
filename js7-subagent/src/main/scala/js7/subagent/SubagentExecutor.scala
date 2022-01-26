package js7.subagent

import js7.agent.data.Problems.{SubagentIdMismatchProblem, SubagentRunIdMismatchProblem}
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{Base64UUID, ProgramTermination, SetOnce}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.{InventoryItem, SignableItem}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.data.value.expression.Expression
import js7.journal.watch.InMemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.SubagentExecutor._
import js7.subagent.client.SubagentDriver
import js7.subagent.configuration.SubagentConf
import js7.subagent.data.SubagentCommand.{CoupleDirector, DedicateSubagent}
import js7.subagent.data.SubagentEvent.{SubagentDedicated, SubagentItemAttached, SubagentShutdown}
import monix.eval.{Fiber, Task}

trait SubagentExecutor
{
  protected[this] val dedicatedOnce: SetOnce[Dedicated]
  protected def journal: InMemoryJournal[SubagentState]
  protected val subagentConf: SubagentConf
  protected def jobLauncherConf: JobLauncherConf
  protected def onStopped(termination: ProgramTermination): Task[Unit]

  private val subagentDriverConf = SubagentDriver.Conf.fromConfig(subagentConf.config)
  private val orderToProcessing = AsyncMap.empty[OrderId, Processing]
  private val subagentRunId = SubagentRunId(Base64UUID.random())

  final def shutdown(
    signal: Option[ProcessSignal] = None,
    restart: Boolean = false)
  : Task[ProgramTermination] =
    journal.persistKeyedEvent(NoKey <-: SubagentShutdown)
      // The event probably gets lost due to immediate shutdown
      .flatMap { checked =>
        for (problem <- checked.left) logger.warn(s"shutdown: $problem")
        dedicatedOnce
          .toOption
          .fold(Task.unit)(_.subagentDriver.stop(signal))
          .flatMap { _ =>
            logger.info(s"Subagent${dedicatedOnce.toOption.fold("")(_.toString + " ")} stopped")
            val t = ProgramTermination(restart = restart)
            onStopped(t).as(t)
          }
      }

  protected def executeDedicateSubagent(cmd: DedicateSubagent)
  : Task[Checked[DedicateSubagent.Response]] =
    Task.defer {
      val isFirst = dedicatedOnce.trySet(
        new Dedicated(
          newLocalSubagentDriver(cmd.subagentId, cmd.agentPath, cmd.controllerId)))
      if (!isFirst) {
        //if (cmd.subagentId == dedicatedOnce.orThrow.subagentId)
        //  Task.pure(Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst)))
        //else
        Task.pure(Left(Problem.pure(
          s"This Subagent has already been dedicated: $dedicatedOnce")))
      } else {
        val event = SubagentDedicated(cmd.subagentId, subagentRunId, cmd.agentPath, cmd.controllerId)
        // TODO Check agentPath, controllerId (handle in SubagentState?)
        journal.persistKeyedEvent(NoKey <-: event)
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
      subagentDriverConf)

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

  protected def attachItem(item: InventoryItem): Task[Checked[Unit]] =
    journal
      .persist(state =>
        Right(if (state.keyToItem.get(item.key).contains(item))
          Nil  // Ignore duplicate
        else {
          if (item.isInstanceOf[SignableItem]) {
            logger.warn(s"❗️ Signature not validated for ${item.key}") // FIXME Validate signature!
          }
          (NoKey <-: SubagentItemAttached(item)) :: Nil
        }))
      .rightAs(())

  protected final def startOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Unit]] =
    orderToProcessing
      .updateChecked(order.id, {
        case Some(existing) =>
          Task.pure(Right(existing)) // Idempotency: Order process has already been started

        case None =>
          Task(dedicatedOnce.checked).flatMapT(dedicated =>
            processOrder(dedicated.subagentDriver, order, defaultArguments)
              .guarantee(orderToProcessing.remove(order.id).void)
              .start
              .map(fiber => Right(Processing(order, fiber))))
      })
      .rightAs(())

  private def processOrder(
    driver: LocalSubagentDriver[SubagentState],
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Outcome] =
    driver
      .processOrder2(order, defaultArguments)
      .onErrorHandle(Outcome.Failed.fromThrowable)
      .flatMap { outcome =>
        val orderProcessed = order.id <-: OrderProcessed(outcome)
        journal
          .persistKeyedEvent(orderProcessed)
          .map(_.onProblemHandle(problem => logger.error(s"${order.id}: $problem")))  // ???
          .as(outcome)
      }
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
    order: Order[Order.Processing],
    fiber: Fiber[Outcome])
}
