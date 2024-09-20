package js7.subagent.job

import cats.effect
import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.launcher.StdObservers
import js7.launcher.internal.JobLauncher

private[subagent] final class JobDriver(params: JobDriver.Params)(using ioRuntime: IORuntime):

  import params.{checkedJobLauncher, jobConf}
  import jobConf.{jobKey, sigkillDelay, workflowJob}

  private val logger = Logger.withPrefix[this.type]("Job:" + jobKey.simpleName)

  override def toString =
    s"JobDriver($jobKey ${workflowJob.executable})"

  // JobDriver consists of two sections: //
  // - mutableSection, to manage all orders and processes //
  // - JobDriverForOrder, for each order //

  private val orderToProcess = AsyncMap.empty[OrderId, JobDriverForOrder]
  @volatile private var lastProcessTerminated: Deferred[IO, Unit] | Null = null

  for launcher <- checkedJobLauncher do
    // TODO JobDriver.start(): IO[Checked[JobDriver]]
    launcher.precheckAndWarn.unsafeRunAndForget()
  for problem <- checkedJobLauncher.left do logger.error(problem.toString)

  def stop(signal: ProcessSignal): IO[Unit] =
    logger.debugIO("stop", signal):
      IO.defer:
        lastProcessTerminated = Deferred.unsafe
        IO.unlessA(orderToProcess.isEmpty):
          killAll(signal) *>
            IO.unlessA(signal == SIGKILL):
              IO.sleep(sigkillDelay) *> killAll(SIGKILL)
            .background.surround:
              lastProcessTerminated.get
          .logWhenItTakesLonger(s"'killing all $jobKey processes'")
        .flatMap: _ =>
          checkedJobLauncher.toOption.fold(IO.unit): jobLauncher =>
            logger.traceIO(s"Stop '$jobLauncher'"):
              jobLauncher
                .stop
                .logWhenItTakesLonger
                .handleError: throwable =>
                  logger.error(s"Stop '$jobLauncher' failed: ${throwable.toStringWithCauses}",
                    throwable.nullIfNoStackTrace)

  private def killAll(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      val drivers = orderToProcess.toMap.values
      if drivers.nonEmpty then
        logger.warn(s"Terminating, sending $signal to $orderProcessCount processes")
      drivers
        .toVector
        .traverse: driver =>
          driver.kill(signal)
        .map(_.combineAll)
        .handleError: t =>
          logger.error(s"killAll: ${t.toStringWithCauses}", t)

  def runOrderProcess(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : IO[OrderOutcome] =
    val forOrder = new JobDriverForOrder(order.id, params)
    IO(checkedJobLauncher)
      .flatTapT: _ =>
        orderToProcess.insert(order.id, forOrder)
      .flatMap:
        case Left(problem) => // insertion failed
          IO.pure(OrderOutcome.Disrupted(problem))

        case Right(jobLauncher: JobLauncher) =>
          forOrder.processOrder(order, executeArguments, stdObservers, jobLauncher)
            .guarantee:
              removeOrderEntry(forOrder)

  private def removeOrderEntry(forOrder: JobDriverForOrder): IO[Unit] =
      orderToProcess.remove(forOrder.orderId) *>
        IO.defer:
          IO.whenA(orderToProcess.isEmpty && lastProcessTerminated != null):
            lastProcessTerminated.complete(()).void

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    IO.defer:
      orderToProcess
        .get(orderId)
        .fold(IO(logger.debug(s"⚠️ killProcess $orderId => no process for Order"))):
          _.killWithSigkillDelay(signal)

  private def orderProcessCount =
    orderToProcess.size


private[subagent] object JobDriver:

  final case class Params(
    jobConf: JobConf,
    pathToJobResource: JobResourcePath => Checked[JobResource],
    checkedJobLauncher: Checked[JobLauncher],
    fileValueState: FileValueState)
