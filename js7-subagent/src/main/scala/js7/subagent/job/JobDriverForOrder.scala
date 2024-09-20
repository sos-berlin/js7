package js7.subagent.job

import cats.effect.{FiberIO, IO, ResourceIO}
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import java.util.Objects.requireNonNull
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.{FiberVar, SyncDeadline}
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderOutcome.Succeeded
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems
import js7.data.subagent.Problems.ProcessCancelledBeforeStartProblem
import js7.data.value.expression.Expression
import js7.launcher.internal.JobLauncher
import js7.launcher.{OrderProcess, ProcessOrder, StdObservers}

private final class JobDriverForOrder(val orderId: OrderId, jobDriverParams: JobDriver.Params):

  import jobDriverParams.{checkedJobLauncher, fileValueState, jobConf, pathToJobResource}
  import jobConf.{jobKey, sigkillDelay, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](s"$orderId Job:${jobConf.jobKey.simpleName}")
  private val _orderProcess = Atomic:
    none[Either[ProcessSignal /*killed before started*/ , OrderProcess]]
  private val sigkillFiber = FiberVar[Unit]()
  private var runningSince: SyncDeadline | Null = null
  private var isKilled = false
  private var sigkilled = false
  private var timedOut = false

  def processOrder(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers,
    jobLauncher: JobLauncher)
  : IO[OrderOutcome] =
    IO(processOrderResource(order, executeArguments, stdObservers))
      .flatMapT(_.use: processOrder_ =>
        processOrder2(jobLauncher, processOrder_))
      .catchIntoChecked
      .map:
        case Left(problem) => OrderOutcome.Disrupted(problem)
        case Right(outcome) => outcome

  private def processOrderResource(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : Checked[ResourceIO[ProcessOrder]] =
    checkedJobLauncher
      // Read JobResources each time because they may change at any time
      .flatMap: _ =>
        jobConf.jobResourcePaths.traverse(pathToJobResource)
      .map: jobResources =>
        ProcessOrder.resource(
          order, workflow, jobKey, workflowJob, jobResources,
          executeArguments,
          workflowJob.defaultArguments,
          jobConf.controllerId, stdObservers,
          fileValueState)

  private def processOrder2(jobLauncher: JobLauncher, processOrder: ProcessOrder)
  : IO[Checked[OrderOutcome]] =
    jobLauncher.startIfNeeded
      .flatMapT: _ =>
        jobLauncher.toOrderProcess(processOrder)
      .flatMapT: orderProcess =>
        _orderProcess.getAndSet(Some(Right(orderProcess))) match
          case None => IO.right(orderProcess)
          case Some(Left(signal)) => // Canceled before start
            cancelProcess(orderProcess, signal)
              .as(Left(ProcessCancelledBeforeStartProblem))
          case Some(Right(_)) => IO(sys.error("Duplicate orderProcess?")) // Does not happen
      .flatMap:
        case Left(ProcessCancelledBeforeStartProblem) =>
          IO.right:
            OrderOutcome.Killed(OrderOutcome.Failed.fromProblem(ProcessCancelledBeforeStartProblem))
        case Left(problem) =>
          IO.left(problem)
        case Right(orderProcess) =>
          IO.defer:
            orderProcess.start(processOrder.order.id, jobKey)
          .flatMap: fiber =>
            SyncDeadline.usingNow: now ?=>
              runningSince = now
            .*>(scheduleTimeoutCancellation)
            .flatMap: timeoutFiber =>
              fiber.joinStd.map(modifyOutcome).mapOrKeep:
                case outcome: Succeeded => readErrorLine(processOrder).getOrElse(outcome)
              .guarantee(timeoutFiber.cancel)
              .guarantee(sigkillFiber.cancel)
          .handleError: t =>
            logger.error(s"${t.toStringWithCauses}", t.nullIfNoStackTrace)
            OrderOutcome.Failed.fromThrowable(t)
          .map(Right(_))

  private def modifyOutcome(outcome: OrderOutcome) =
    outcome match
      case outcome: OrderOutcome.Completed =>
        if timedOut then
          OrderOutcome.TimedOut(outcome)
        else if isKilled then
          OrderOutcome.Killed(outcome)
        else
          outcome
      case o => o

  private def readErrorLine(processOrder: ProcessOrder): Option[OrderOutcome.Failed] =
    processOrder.stdObservers.errorLine
      .map: errorLine =>
        assert(workflowJob.failOnErrWritten) // see OrderActor
        OrderOutcome.Failed(Some(s"The job's error channel: $errorLine"))

  private def scheduleTimeoutCancellation: IO[FiberIO[Unit]] =
    IO.defer:
      requireNonNull(runningSince)
      workflowJob.timeout.fold(IO.unit): timeout =>
        IO.sleep(timeout)
          .flatMap: _ =>
            SyncDeadline.usingNow:
              timedOut = true
              logger.warn(s"OrderProcess has timed out after ${
                runningSince.elapsed.pretty} and will be killed now")
          .flatMap: _ =>
            killWithSigkillDelay(SIGTERM)
              .handleError: t =>
                logger.error(s"killOrderAndForget => ${t.toStringWithCauses}", t)
              .startAndForget
      .start

  def killWithSigkillDelay(signal_ : ProcessSignal): IO[Unit] =
    IO.defer:
      val signal = if sigkillDelay.isZeroOrBelow then SIGKILL else signal_
      kill(signal) *>
        IO.whenA(signal != SIGKILL && _orderProcess.get.exists(_.isRight) /*OrderProcess?*/):
          IO.sleep(sigkillDelay).productR:
            IO.defer:
              logger.warn(s"SIGKILL now, because sigkillDelay elapsed")
              kill(SIGKILL)
          .start
          .flatMap(sigkillFiber.set)

  def kill(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      IO.unlessA(signal == SIGKILL && sigkilled):
        _orderProcess.compareAndExchange(None, Some(Left(signal))) match
          case None => IO(logger.debug(s"⚠️ kill($signal) BEFORE STARTED"))
          case Some(Left(signal)) =>
            IO(logger.debug(s"Already killed with $signal"))
          case Some(Right(orderProcess)) =>
            IO.defer:
              logger.debug(s"Kill $signal")
              isKilled = true
              sigkilled |= signal == SIGKILL
              cancelProcess(orderProcess, signal)

  private def cancelProcess(orderProcess: OrderProcess, signal: ProcessSignal): IO[Unit] =
    orderProcess.cancel(immediately = signal == SIGKILL)
      .handleError: t =>
        logger.error(s"Kill $signal => ${t.toStringWithCauses}", t.nullIfNoStackTrace)
