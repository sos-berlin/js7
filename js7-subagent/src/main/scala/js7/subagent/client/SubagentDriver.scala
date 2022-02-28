package js7.subagent.client

import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.JournaledState
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId}
import js7.data.state.AgentStateView
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.Execute
import js7.journal.CommitOptions
import js7.journal.state.StatePersistence
import js7.subagent.client.SubagentDriver._
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

trait SubagentDriver
{
  protected type S <: AgentStateView with JournaledState[S]

  def subagentId: SubagentId

  def isHeartbeating: Boolean

  protected def persistence: StatePersistence[S]

  protected val conf: SubagentDriver.Conf

  def start: Task[Unit]

  def stop(signal: Option[ProcessSignal]): Task[Unit]

  def processOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]]

  def continueProcessingOrder(order: Order[Order.Processing]): Task[Checked[OrderProcessed]]

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit]

  private val delayCommit = CommitOptions(delay = conf.stdoutCommitDelay)

  protected final def persistStdouterr(orderId: OrderId, t: StdoutOrStderr, chunk: String)
  : Task[Unit] =
    persistence
      .persistKeyedEventsLater((orderId <-: OrderStdWritten(t)(chunk)) :: Nil, delayCommit)
      .map {
        case Left(problem) => logger.error(s"Emission of OrderStdWritten event failed: $problem")
        case Right(_) =>
      }

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
  private val logger = Logger[this.type]

  final case class Conf(
    stdoutCommitDelay: FiniteDuration,
    charBufferSize: Int,
    stdouterr: StdouterrConf,
    defaultJobSigkillDelay: FiniteDuration)
  object Conf {
    def fromConfig(config: Config) = {
      val outErrConf = StdouterrConf.fromConfig(config)
      new Conf(
        stdoutCommitDelay = config.getDuration("js7.order.stdout-stderr.commit-delay")
          .toFiniteDuration,
        charBufferSize = config.memorySizeAsInt("js7.order.stdout-stderr.char-buffer-size")
          .orThrow.min(outErrConf.chunkSize),
        outErrConf,
        defaultJobSigkillDelay = config.getDuration("js7.job.execution.sigkill-delay")
          .toFiniteDuration)
    }
  }

  final case class StdouterrConf(chunkSize: Int, delay: FiniteDuration)
  object StdouterrConf {
    def fromConfig(config: Config): StdouterrConf = new StdouterrConf(
      chunkSize = config.memorySizeAsInt("js7.order.stdout-stderr.chunk-size").orThrow,
      delay = config.getDuration("js7.order.stdout-stderr.delay").toFiniteDuration)
  }
}
