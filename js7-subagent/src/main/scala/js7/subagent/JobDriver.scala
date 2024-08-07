package js7.subagent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, ResourceIO}
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import java.util.Objects.requireNonNull
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.{FiberVar, SyncDeadline}
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.OrderOutcome.Succeeded
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems
import js7.data.subagent.Problems.ProcessCancelledBeforeStartProblem
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.launcher.internal.JobLauncher
import js7.launcher.{OrderProcess, ProcessOrder, StdObservers}
import js7.subagent.JobDriver.*

private final class JobDriver(
  jobConf: JobConf,
  pathToJobResource: JobResourcePath => Checked[JobResource],
  checkedJobLauncher: Checked[JobLauncher],
  fileValueState: FileValueState)
  (using ioRuntime: IORuntime):

  import jobConf.{jobKey, sigkillDelay, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.name)
  private val orderToProcess = AsyncMap.empty[OrderId, Entry]
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
            logger.traceIO("JobLauncher stop"):
              jobLauncher
                .stop
                .logWhenItTakesLonger
                .handleError: throwable =>
                  logger.error(s"Stop '$jobLauncher' failed: ${throwable.toStringWithCauses}",
                    throwable.nullIfNoStackTrace)

  /** Starts the process and returns a Fiber returning the process' outcome. */
  def runOrderProcess(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : IO[OrderOutcome] =
    val entry = new Entry(order.id)
    IO(checkedJobLauncher)
      .flatTapT(_ =>
        orderToProcess.insert(order.id, entry))
      .flatMap:
        case Left(problem) =>
          IO.pure(OrderOutcome.Disrupted(problem): OrderOutcome)

        case Right(jobLauncher: JobLauncher) =>
          processOrder(order, executeArguments, stdObservers, jobLauncher, entry)
            .guarantee(removeEntry(entry))

  private def processOrder(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers,
    jobLauncher: JobLauncher,
    entry: Entry)
  : IO[OrderOutcome] =
    IO(processOrderResource(order, executeArguments, stdObservers))
      .flatMapT(_.use(processOrder_ =>
        processOrder2(jobLauncher, processOrder_, entry)))
      .catchIntoChecked
      .map:
        case Left(problem) => OrderOutcome.Disrupted(problem)
        case Right(outcome) => outcome

  private def processOrder2(jobLauncher: JobLauncher, processOrder: ProcessOrder, entry: Entry)
  : IO[Checked[OrderOutcome]] =
    jobLauncher.startIfNeeded
      .flatMapT: _ =>
        jobLauncher.toOrderProcess(processOrder)
      .flatMapT: orderProcess =>
        entry.orderProcess.getAndSet(Some(Right(orderProcess))).match
          case None => IO.right(orderProcess)
          case Some(Left(signal)) => // Canceled before start
            cancelProcess(entry.orderId, orderProcess, signal)
              .as(Left(ProcessCancelledBeforeStartProblem))
          case Some(Right(_)) => IO(sys.error("Duplicate orderProcess?")) // Does not happen
      .flatMap:
        case Left(ProcessCancelledBeforeStartProblem) => IO.right:
          OrderOutcome.Killed(OrderOutcome.Failed.fromProblem(ProcessCancelledBeforeStartProblem))
        case Left(problem) => IO.left(problem)
        case Right(orderProcess) =>
          IO.defer:
            orderProcess.start(processOrder.order.id, jobKey)
          .flatMap: fiber =>
            SyncDeadline.usingNow: now ?=>
              entry.runningSince = now
            .*>(scheduleTimeoutCancellation(entry))
            .*>(IO.defer:
              fiber.joinStd
                .map(entry.modifyOutcome)
                .mapOrKeep:
                  case outcome: Succeeded => readErrorLine(processOrder).getOrElse(outcome))
          .flatTap: _ =>
            entry.sigkillFiber.cancel
          .handleError: t =>
            logger.error(s"${processOrder.order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
            OrderOutcome.Failed.fromThrowable(t)
          .map(Right(_))

  private def processOrderResource(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : Checked[ResourceIO[ProcessOrder]] =
    checkedJobLauncher
      // Read JobResources each time because they may change at any time
      .flatMap(_ => jobConf.jobResourcePaths.traverse(pathToJobResource))
      .map(jobResources =>
        ProcessOrder.resource(
          order, workflow, jobKey, workflowJob, jobResources,
          executeArguments,
          workflowJob.defaultArguments,
          jobConf.controllerId, stdObservers,
          fileValueState))

  private def scheduleTimeoutCancellation(entry: Entry): IO[Unit] =
    IO.defer:
      requireNonNull(entry.runningSince)
      workflowJob.timeout.fold(IO.unit): timeout =>
        IO.sleep(timeout)
          .flatMap: _ =>
            SyncDeadline.usingNow:
              entry.timedOut = true
              logger.warn(s"${entry.orderId}: OrderProcess has timed out after ${
                entry.runningSince.elapsed.pretty} and will be killed now")
          .flatMap: _ =>
            killOrderAndForget(entry, SIGTERM)
        .start
        .flatMap(entry.timeoutFiber.set)

  private def readErrorLine(processOrder: ProcessOrder): Option[OrderOutcome.Failed] =
    processOrder.stdObservers.errorLine
      .map: errorLine =>
        assert(workflowJob.failOnErrWritten) // see OrderActor
        OrderOutcome.Failed(Some(s"The job's error channel: $errorLine"))

  private def removeEntry(entry: Entry): IO[Unit] =
    entry.timeoutFiber.cancel *>
      orderToProcess
        .remove(entry.orderId)
        .flatMap: _ =>
          IO.whenA(orderToProcess.isEmpty && lastProcessTerminated != null):
            lastProcessTerminated.complete(()).void

  private def killOrderAndForget(entry: Entry, signal: ProcessSignal): IO[Unit] =
    killOrder(entry, signal)
      .handleError: t =>
        logger.error(s"${entry.orderId}: ${t.toStringWithCauses}", t)
      .startAndForget

  def killOrder(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    IO.defer:
      orderToProcess
        .get(orderId)
        .fold(IO(logger.debug(s"⚠️ $orderId: killOrder $orderId => no process for Order"))):
          killOrder(_, signal)

  private def killOrder(entry: Entry, signal_ : ProcessSignal): IO[Unit] =
    IO.defer:
      val signal = if sigkillDelay.isZeroOrBelow then SIGKILL else signal_
      killProcess(entry, signal) *>
        IO.whenA(signal != SIGKILL && entry.orderProcess.get.exists(_.isRight)/*OrderProcess?*/):
          (IO.sleep(sigkillDelay) *>
            IO.defer:
              logger.warn(s"${entry.orderId}: SIGKILL after sigkillDelay elapsed")
              killProcess(entry, SIGKILL))
            .start
            .flatMap(entry.sigkillFiber.set)

  private def killAll(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      val entries = orderToProcess.toMap.values
      if entries.nonEmpty then
        logger.warn(s"Terminating, sending $signal to $orderProcessCount processes")
      entries
        .toVector
        .traverse(killProcess(_, signal))
        .map(_.combineAll)
        .handleError: t =>
          logger.error(s"killAll: ${t.toStringWithCauses}", t)

  private def killProcess(entry: Entry, signal: ProcessSignal): IO[Unit] =
    import entry.orderId
    IO.defer:
      IO.unlessA(signal == SIGKILL && entry.sigkilled):
        entry.orderProcess.compareAndExchange(None, Some(Left(signal))) match
          case None => IO(logger.debug(s"⚠️ $orderId: killProcess($signal) BEFORE STARTED"))
          case Some(Left(signal)) => IO.unit // Ignore duplicate kill before started
          case Some(Right(orderProcess)) =>
            IO.defer:
              logger.debug(s"$orderId: Kill $signal")
              entry.isKilled = true
              entry.sigkilled |= signal == SIGKILL
              cancelProcess(orderId, orderProcess, signal)

  private def cancelProcess(orderId: OrderId, orderProcess: OrderProcess, signal: ProcessSignal)
  : IO[Unit] =
    IO.defer:
      orderProcess.cancel(immediately = signal == SIGKILL)
    .handleError(t => logger.error(
      s"$orderId: Kill $signal => ${t.toStringWithCauses}", t.nullIfNoStackTrace))

  override def toString = s"JobDriver($jobKey ${workflowJob.executable})"

  private def orderProcessCount = orderToProcess.size


private object JobDriver:

  private final class Entry(val orderId: OrderId):
    val orderProcess = Atomic(none[Either[ProcessSignal/*killed before started*/, OrderProcess]])
    val timeoutFiber = FiberVar[Unit]()
    val sigkillFiber = FiberVar[Unit]()
    var runningSince: SyncDeadline | Null = null
    var isKilled = false
    var sigkilled = false
    var timedOut = false

    def modifyOutcome(outcome: OrderOutcome) =
      outcome match
        case outcome: OrderOutcome.Completed =>
          if timedOut then
            OrderOutcome.TimedOut(outcome)
          else if isKilled then
            OrderOutcome.Killed(outcome)
          else
            outcome
        case o => o
