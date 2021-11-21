package js7.subagent

import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{ProgramTermination, SetOnce}
import js7.data.controller.ControllerId
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.journal.watch.InMemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.SubagentExecutor._
import js7.subagent.client.SubagentDriver
import js7.subagent.configuration.SubagentConf
import monix.eval.{Fiber, Task}
import monix.execution.Scheduler

trait SubagentExecutor
{
  protected[subagent] val dedicated: SetOnce[Dedicated]
  protected def scheduler: Scheduler
  protected def journal: InMemoryJournal[SubagentState]
  protected def subagentConf: SubagentConf
  protected def jobLauncherConf: JobLauncherConf
  protected def onStopped(termination: ProgramTermination): Task[Unit]

  private val subagentDriverConf = SubagentDriver.Conf.fromConfig(subagentConf.config)
  private val orderToEntry = AsyncMap.empty[OrderId, OrderEntry]

  implicit private def implicitScheduler = scheduler

  final def stop(
    signal: Option[ProcessSignal] = None,
    restart: Boolean = false)
  : Task[ProgramTermination] =
    Task.defer {
      //TODO stopped = true, beware race-condition
      dedicated.toOption.fold(Task.unit)(_
        .subagentDriver
        .stop(signal))
        .flatMap { _ =>
          val t = ProgramTermination(restart = restart)
          onStopped(t).as(t)
        }
    }

  protected final def newLocalSubagentDriver(subagentId: SubagentId, controllerId: ControllerId) =
    new LocalSubagentDriver(
      subagentId,
      () => journal.currentState,
      persistStdouterr,
      controllerId,
      jobLauncherConf,
      subagentDriverConf)

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
          dedicated.task.flatMap(_
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

  private final case class OrderEntry(
    order: Order[Order.Processing],
    fiber: Fiber[Outcome])
}
