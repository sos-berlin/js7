package js7.subagent.client

import cats.syntax.traverse._
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.io.process.{ProcessSignal, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.chunkStrings
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.launcher.StdObservers
import js7.subagent.LocalSubagentDriver.StdouterrConf
import js7.subagent.OutErrStatistics
import js7.subagent.client.SubagentDriver._
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration

trait SubagentDriver
{
  def subagentId: SubagentId

  def isAlive: Boolean

  def start: Task[Unit]

  def stop(signal: Option[ProcessSignal]): Task[Unit]

  protected def conf: Conf

  def processOrder(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Outcome]

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit]

  protected final def withStdObservers[A](
    orderId: OrderId,
    keepLastErrLine: Boolean,
    onStdouterr: (OrderId, StdoutOrStderr, String) => Task[Unit])
    (body: StdObservers => Task[A])
  : Task[A] =
    Task.defer {
      val outErrStatistics = Map(
        Stdout -> new OutErrStatistics,
        Stderr -> new OutErrStatistics)

      val observingStarted = Map(
        Stdout -> Promise[Unit](),
        Stderr -> Promise[Unit]())

      val out, err = PublishSubject[String]()
      val stdObservers = new StdObservers(out, err, charBufferSize = conf.charBufferSize,
        keepLastErrLine = keepLastErrLine)

      def writeObservableAsEvents(outerr: StdoutOrStderr, observable: Observable[String]) =
        observable
          .doAfterSubscribe(Task(observingStarted(outerr).success(())))
          .buffer(Some(conf.stdouterr.delay), conf.stdouterr.chunkSize, toWeight = _.length)
          .flatMap(strings => Observable.fromIterable(chunkStrings(strings, conf.stdouterr.chunkSize)))
          .flatMap(chunk => Observable.fromTask(
            outErrStatistics(outerr).count(
              chunk.length,
              onStdouterr(orderId, outerr, chunk))))
          .completedL

      val observeOutErr = Task
        .parZip2(
          writeObservableAsEvents(Stdout, out),
          writeObservableAsEvents(Stderr, err))
        .void

      for {
        observingOutErr <- observeOutErr.start
        _ <- observingStarted.values.toSeq.traverse(promise => Task.fromFuture(promise.future))
        result <- body(stdObservers)
        _ <- stdObservers.stop /*may already have been stopped by OrderProcess/JobDriver*/
        _ <- observingOutErr.join
      } yield {
        if (outErrStatistics(Stdout).isRelevant || outErrStatistics(Stderr).isRelevant) {
          logger.debug(s"stdout: ${outErrStatistics(Stdout)}, stderr: ${outErrStatistics(Stderr)}")
        }
        result
      }
    }
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
      val outErrConf = StdouterrConf(config)
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

}
