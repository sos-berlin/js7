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
import js7.journal.state.StatePersistence
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

  protected def persistence: StatePersistence[S]

  protected val conf: SubagentDriver.Conf

  def start: Task[Unit]

  def stop(signal: Option[ProcessSignal]): Task[Unit]

  def tryShutdown: Task[Unit]

  def processOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]]

  def continueProcessingOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]]

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit]

  final def orderToExecuteDefaultArguments(order: Order[Order.Processing]) =
    persistence.state
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
