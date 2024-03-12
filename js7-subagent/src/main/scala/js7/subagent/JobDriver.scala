package js7.subagent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Resource}
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import java.util.Objects.requireNonNull
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.SyncDeadline
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.*
import js7.base.monixlike.SerialSyncCancelable
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.OrderOutcome.Succeeded
import js7.data.order.{Order, OrderId, OrderOutcome}
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

  import ioRuntime.scheduler
  import jobConf.{jobKey, sigkillDelay, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.name)
  private val orderToProcess = AsyncMap.empty[OrderId, Entry]
  @volatile private var lastProcessTerminated: Deferred[IO, Unit] = null

  for launcher <- checkedJobLauncher do
    // TODO JobDriver.start(): IO[Checked[JobDriver]]
    launcher.precheckAndWarn.unsafeRunAndForget()

  for problem <- checkedJobLauncher.left do logger.error(problem.toString)

  def stop(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      logger.debug("Stop")
      lastProcessTerminated = Deferred.unsafe
      (if orderToProcess.isEmpty then
        IO.unit
      else
        killAll(signal)
          .flatMap: _ =>
            IO.unlessA(signal == SIGKILL):
              (IO.sleep(sigkillDelay) *> killAll(SIGKILL))
                // TODO Cancel Fiber when process has terminated before sigkillDelay
                .startAndForget
          .flatMap: _ =>
            lastProcessTerminated.get
          .logWhenItTakesLonger(s"'killing all $jobKey processes'")
      ).flatMap(_ =>
        checkedJobLauncher.toOption.fold(IO.unit) { jobLauncher =>
          logger.trace("JobLauncher stop")
          jobLauncher
            .stop
            .logWhenItTakesLonger
            .handleError(throwable =>
              logger.error(s"Stop '$jobLauncher' failed: ${throwable.toStringWithCauses}",
                throwable.nullIfNoStackTrace))
        })

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
      .flatMapT(_ => jobLauncher.toOrderProcess(processOrder))
      .flatMapT { orderProcess =>
        entry.orderProcess = Some(orderProcess)
        // Start the orderProcess. The future completes the stdObservers (stdout, stderr)
        orderProcess
          .start(processOrder.order.id, jobKey)
          .flatMap { runningProcess =>
            val maybeKillAfterStart = entry.killSignal.traverse(killOrder(entry, _))
            val awaitTermination =
              SyncDeadline.usingNow: now ?=>
                entry.runningSince = now
                scheduleTimeout(entry)
              .*>(IO.defer:
                runningProcess.joinStd
                  .map(entry.modifyOutcome)
                  .map:
                    case outcome: Succeeded => readErrorLine(processOrder).getOrElse(outcome)
                    case outcome => outcome)
            IO.both(maybeKillAfterStart, awaitTermination)
              .map((_, outcome) => outcome)
          }
          .handleError { t =>
            logger.error(s"${processOrder.order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
            OrderOutcome.Failed.fromThrowable(t)
          }
          .map(Right(_))
      }

  private def processOrderResource(
    order: Order[Order.Processing],
    executeArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : Checked[Resource[IO, ProcessOrder]] =
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

  private def scheduleTimeout(entry: Entry)(using SyncDeadline.Now): Unit =
    requireNonNull(entry.runningSince)
    for t <- workflowJob.timeout do
      entry.timeoutSchedule := scheduler.scheduleOnce(t):
        entry.timedOut = true
        logger.warn("OrderProcess for " + entry.orderProcess + " has been timed out after " +
          entry.runningSince.elapsed.pretty + " and will be killed now")
        killOrderAndForget(entry, SIGTERM)

  private def readErrorLine(processOrder: ProcessOrder): Option[OrderOutcome.Failed] =
    processOrder.stdObservers.errorLine
      .map { errorLine =>
        assert(workflowJob.failOnErrWritten) // see OrderActor
        OrderOutcome.Failed(Some(s"The job's error channel: $errorLine"))
      }

  private def removeEntry(entry: Entry): IO[Unit] =
    IO.defer:
      import entry.orderId
      entry.timeoutSchedule.cancel()
      orderToProcess
        .remove(orderId)
        .flatMap: _ =>
          IO.whenA(orderToProcess.isEmpty && lastProcessTerminated != null):
            lastProcessTerminated.complete(()).void

  private def killOrderAndForget(entry: Entry, signal: ProcessSignal): Unit =
    killOrder(entry, signal)
      .handleError(t =>
        logger.error(t.toStringWithCauses + " - " + entry.orderId, t))
      .unsafeRunAndForget()

  def killOrder(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    IO.defer(
      orderToProcess
        .get(orderId)
        .fold(IO(logger.debug(s"⚠️ killOrder $orderId => no process for Order")))(
          killOrder(_, signal)))

  private def killOrder(entry: Entry, signal_ : ProcessSignal): IO[Unit] =
    IO.defer:
      val signal = if sigkillDelay.isZeroOrBelow then SIGKILL else signal_
      entry.killSignal = Some(signal)
      killProcess(entry, signal)
        .flatMap: _ =>
          IO.unlessA(signal == SIGKILL):
            (IO.sleep(sigkillDelay) *> killProcess(entry, SIGKILL))
              // TODO Cancel Fiber when process has terminated before sigkillDelay
              .startAndForget

  private def killAll(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      val entries = orderToProcess.toMap.values
      if entries.nonEmpty then logger.warn(
        s"Terminating, sending $signal to $orderProcessCount processes")
      entries
        .toVector
        .traverse(killProcess(_, signal))
        .map(_.combineAll)
        .handleError(t =>
          logger.error(t.toStringWithCauses, t))

  private def killProcess(entry: Entry, signal: ProcessSignal): IO[Unit] =
    IO.defer:
      if  signal == SIGKILL && entry.sigkilled then
        IO.unit
      else
        entry.orderProcess match
          case None =>
            logger.debug(s"killProcess(${entry.orderId},  $signal): no OrderProcess")
            IO.unit
          case Some(orderProcess) =>
            logger.debug(s"Kill $signal ${entry.orderId}")
            entry.isKilled = true
            entry.sigkilled |= signal == SIGKILL
            IO
              .defer/*catch inside io*/ :
                orderProcess.cancel(immediately = signal == SIGKILL)
              .handleError(t => logger.error(
                s"Kill ${entry.orderId}}: ${t.toStringWithCauses}", t.nullIfNoStackTrace))

  override def toString = s"JobDriver($jobKey ${workflowJob.executable})"

  private def orderProcessCount = orderToProcess.size

private object JobDriver:
  private final class Entry(val orderId: OrderId):
    var orderProcess: Option[OrderProcess] = None
    var killSignal: Option[ProcessSignal] = None
    val timeoutSchedule = SerialSyncCancelable()
    var runningSince: SyncDeadline = null
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
