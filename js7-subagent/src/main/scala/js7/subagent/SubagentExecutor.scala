package js7.subagent

import js7.agent.data.Problems.{SubagentIdMismatchProblem, SubagentNotDedicatedProblem, SubagentRunIdMismatchProblem}
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{Base64UUID, ProgramTermination, SetOnce}
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.{InventoryItem, SignableItem}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
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
import monix.execution.Scheduler

trait SubagentExecutor
{
  protected[subagent] val dedicatedOnce: SetOnce[Dedicated]
  protected def scheduler: Scheduler
  protected def journal: InMemoryJournal[SubagentState]
  protected def subagentConf: SubagentConf
  protected def jobLauncherConf: JobLauncherConf
  protected def onStopped(termination: ProgramTermination): Task[Unit]

  private val subagentDriverConf = SubagentDriver.Conf.fromConfig(subagentConf.config)
  private val orderToEntry = AsyncMap.empty[OrderId, OrderEntry]
  private val subagentRunId = SubagentRunId(Base64UUID.random())

  implicit private def implicitScheduler = scheduler

  final def shutdown(
    signal: Option[ProcessSignal] = None,
    restart: Boolean = false)
  : Task[ProgramTermination] =
    journal.persistKeyedEvent(NoKey <-: SubagentShutdown)
      // The event probably gets lost due to immediate shutdown
      .flatMap { checked =>
        for (problem <- checked.left) logger.warn(s"shutdown: $problem")
        dedicatedOnce.toOption.fold(Task.unit)(_
          .subagentDriver
          .stop(signal))
          .flatMap { _ =>
            val t = ProgramTermination(restart = restart)
            onStopped(t).as(t)
          }
      }

  protected def executeDedicateSubagent(cmd: DedicateSubagent)
  : Task[Checked[DedicateSubagent.Response]] =
    Task.defer {
      val isFirst = dedicatedOnce.trySet(
        new Dedicated(
          newLocalSubagentDriver(cmd.subagentId, cmd.controllerId)))
      if (!isFirst)
        Task.pure(Left(Problem.pure(
          s"This Subagent has already been dedicated to $dedicatedOnce")))
      else {
        val event = SubagentDedicated(cmd.subagentId, subagentRunId, cmd.agentPath, cmd.controllerId)
        // TODO Check agentPath, controllerId (handle in SubagentState?)
        journal.persistKeyedEvent(NoKey <-: event)
          .rightAs(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst))
      }
    }

  private def newLocalSubagentDriver(subagentId: SubagentId, controllerId: ControllerId) =
    new LocalSubagentDriver(
      subagentId,
      () => journal.currentState,
      persistStdouterr,
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
    dedicatedOnce.toOption match {
      case None => Left(SubagentNotDedicatedProblem)
      case Some(dedicated) =>
        if (requestedSubagentId != dedicated.subagentId)
          Left(SubagentIdMismatchProblem(requestedSubagentId, dedicated.subagentId))
        else
          RightUnit
      }

  private def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
    dedicatedOnce.toOption match {
      case None => Left(SubagentNotDedicatedProblem)
      case Some(dedicated) =>
        if (requestedSubagentRunId != this.subagentRunId) {
          val problem = SubagentRunIdMismatchProblem(dedicated.subagentId)
          logger.warn(
            s"$problem, requestedSubagentRunId=$requestedSubagentRunId, " +
              s"agentRunId=${this.subagentRunId}")
          Left(problem)
        } else
          Checked.unit
      }

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

  private def persistStdouterr(orderId: OrderId, t: StdoutOrStderr, chunk: String): Task[Unit] =
    journal
      .persistKeyedEvent(
        orderId <-: OrderStdWritten(t)(chunk))
      .map {
        case Left(problem) => logger.error(s"Emission of OrderStdWritten event failed: $problem")
        case Right(_) =>
      }

  protected final def startOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Unit]] =
    orderToEntry
      .updateChecked(order.id, {
        case Some(existing) =>
          Task.pure(
            if (order != existing.order)
              Left(Problem(s"Duplicate StartOrderProcess command with different ${order.id}"))
            else
              Right(existing)) // Idempotency: Order process has already been started

        case None =>
          dedicatedOnce.task.flatMap(_
            .subagentDriver
            .processOrder(order, defaultArguments)
            .onErrorHandle(Outcome.Failed.fromThrowable)
            .flatMap(outcome =>
              journal
                .persistKeyedEvent(order.id <-: OrderProcessed(outcome))
                .map {
                  case Left(problem) => logger.error(problem.toString)  // ???
                  case Right(_) =>
                }
                .as(outcome)
            )
            .guarantee(orderToEntry.remove(order.id).void)
            .start
            .map(Checked(_))
            .map(_.map(fiber => OrderEntry(order, fiber))))
      })
      .rightAs(())
}

object SubagentExecutor
{
  private val logger = Logger(getClass)

  private[subagent] final class Dedicated(
    val subagentDriver: LocalSubagentDriver)
  {
    def subagentId = subagentDriver.subagentId

    override def toString = s"Dedicated($subagentId)"
  }

  private final case class OrderEntry(
    order: Order[Order.Processing],
    fiber: Fiber[Outcome])
}
