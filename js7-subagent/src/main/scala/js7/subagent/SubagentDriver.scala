package js7.subagent

import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.io.process.ProcessSignal
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentDriverState, SubagentId}
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.Execute
import js7.journal.state.StateJournal
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

trait SubagentDriver
{
  protected type S <: SubagentDriverState[S]

  def subagentId: SubagentId

  protected def isStopping: Boolean

  protected def isShuttingDown: Boolean

  def isCoupled =
    !isStopping && !isShuttingDown

  protected def journal: StateJournal[S]

  protected val conf: SubagentDriver.Conf

  def start: Task[Unit]

  def stop(signal: Option[ProcessSignal]): Task[Unit]

  def tryShutdown: Task[Unit]

  def processOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]]

  def recoverOrderProcessing(order: Order[Order.Processing]): Task[Checked[OrderProcessed]]

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit]

  // TODO Emit one batch for all recovered orders!
  final def emitOrderProcessLost(order: Order[Order.Processing])
  : Task[Checked[OrderProcessed]] =
    journal
      .persistKeyedEvent(order.id <-: OrderProcessed.processLostDueToRestart)
      .map(_.map(_._1.value.event))

  final def orderToExecuteDefaultArguments(order: Order[Order.Processing])
  : Task[Checked[Map[String, Expression]]] =
    journal.state
      .map(_
        .idToWorkflow
        .checked(order.workflowId)
        .map(_.instruction(order.position))
        .map {
          case o: Execute.Named => o.defaultArguments
          case _ => Map.empty[String, Expression]
        })
}

object SubagentDriver
{
  final case class Conf(
    //recouplingStreamReader: RecouplingStreamReaderConf,
    eventBufferDelay: FiniteDuration,
    eventBufferSize: Int,
    commitDelay: FiniteDuration)
  object Conf {
    def fromConfig(config: Config, commitDelay: FiniteDuration) = {
      new Conf(
        eventBufferDelay  = config.finiteDuration("js7.subagent-driver.event-buffer-delay").orThrow,
        eventBufferSize   = config.getInt        ("js7.subagent-driver.event-buffer-size"),
        commitDelay = commitDelay)
    }
  }

  final case class StdouterrConf(chunkSize: Int, delay: FiniteDuration)
  object StdouterrConf {
    def fromConfig(config: Config): StdouterrConf = new StdouterrConf(
      chunkSize = config.memorySizeAsInt("js7.order.stdout-stderr.chunk-size").orThrow,
      delay = config.getDuration("js7.order.stdout-stderr.delay").toFiniteDuration)
  }
}
