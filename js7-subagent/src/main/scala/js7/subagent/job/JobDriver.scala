package js7.subagent.job

import cats.effect
import cats.effect.{Deferred, IO}
import js7.base.catsutils.CatsExtensions.ifTrue
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessKilledDueToSubagentShutdownProblem
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.launcher.StdObservers
import js7.launcher.internal.JobLauncher

private[subagent] final class JobDriver private(params: JobDriver.Params):

  import params.{checkedJobLauncher, jobConf}
  import jobConf.{jobKey, sigkillDelay, workflowJob}

  private val logger = Logger.withPrefix[this.type]("Job:" + jobKey.simpleName)

  override def toString =
    s"JobDriver($jobKey ${workflowJob.executable})"

  // JobDriver consists of two sections: //
  // - mutableSection, to manage all orders and processes //
  // - JobDriverForOrder, for each order //

  private val orderToProcess = AsyncMap.empty[OrderId, JobDriverForOrder]
  private val lastProcessTerminated = SetOnce[Deferred[IO, Unit]]

  private def precheckAndWarn: IO[Unit] =
    IO(checkedJobLauncher)
      .flatMapT:
        _.precheck
      .flatMap:
        case Left(problem) => IO(logger.warn(problem.toString))
        case Right(()) => IO.unit

  def stop(signal: ProcessSignal): IO[Unit] =
    logger.debugIO("stop", signal):
      IO.defer:
        val deferred = Deferred.unsafe[IO, Unit]
        lastProcessTerminated := deferred
        IO.unlessA(orderToProcess.isEmpty):
          killAllDueToShutdown(signal) *>
            IO.unlessA(signal == SIGKILL):
              IO.sleep(sigkillDelay) *> killAllDueToShutdown(SIGKILL)
            .background.surround:
              deferred.get
          .logWhenItTakesLonger(s"'killing all $jobKey processes'")
        .flatMap: _ =>
          checkedJobLauncher.toOption.fold(IO.unit): jobLauncher =>
            logger.traceIO(s"Stop '$jobLauncher'"):
              jobLauncher
                .stop
                .logWhenItTakesLonger("jobLauncher.stop")
                .handleError: throwable =>
                  logger.error(s"Stop '$jobLauncher' failed: ${throwable.toStringWithCauses}",
                    throwable.nullIfNoStackTrace)

  private def killAllDueToShutdown(signal: ProcessSignal): IO[Unit] =
    orderToProcess.toMap.map(_.values).flatMap: drivers =>
      if drivers.nonEmpty then
        logger.warn(s"Terminating, sending $signal to $orderProcessCount processes")
      drivers.foldMap: driver =>
        driver.kill(signal, processLost = Some(ProcessKilledDueToSubagentShutdownProblem(_)))
      .handleError: t =>
        logger.error(s"killAllDueToShutdown: ${t.toStringWithCauses}", t)

  def runOrderProcess(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp],
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
          forOrder.processOrder(order, executeArguments, endOfAdmissionPeriod, stdObservers, jobLauncher)
            .guarantee:
              removeOrderEntry(forOrder)

  private def removeOrderEntry(forOrder: JobDriverForOrder): IO[Unit] =
    orderToProcess.remove(forOrder.orderId) *>
      IO(orderToProcess.isEmpty).ifTrue:
        lastProcessTerminated.fold(IO.unit):
          _.complete(()).void

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    IO.defer:
      orderToProcess
        .get(orderId)
        .fold(IO(logger.debug(s"⚠️ killProcess $orderId => no process for Order"))):
          _.killWithSigkillDelay(signal)

  private def orderProcessCount =
    orderToProcess.size


private[subagent] object JobDriver:

  def start(params: JobDriver.Params): IO[JobDriver] =
    for
      jobDriver <- IO(new JobDriver(params))
      _ <- jobDriver.precheckAndWarn
    yield
      jobDriver

  final case class Params(
    jobConf: JobConf,
    pathToJobResource: JobResourcePath => Checked[JobResource],
    checkedJobLauncher: Checked[JobLauncher],
    fileValueState: FileValueState)
