package js7.subagent

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.traverse.*
import java.nio.file.Path
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.configutils.Configs.RichConfig
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.{MainService, Service}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.base.web.Uri
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.session.SessionRegister
import js7.common.pekkoutils.Pekkos
import js7.common.system.ThreadPools.unlimitedExecutionContextResource
import js7.data.event.EventId
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.Problems.{SubagentAlreadyDedicatedProblem, SubagentNotDedicatedProblem}
import js7.data.subagent.SubagentCommand.DedicateSubagent
import js7.data.subagent.{SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.journal.MemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.subagent.Subagent.*
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import org.apache.pekko.actor.ActorSystem
import scala.collection.mutable

final class Subagent private(
  val webServer: PekkoWebServer,
  directorRouteVariable: DirectorRouteVariable,
  toForDirector: Subagent => ForDirector,
  val journal: MemoryJournal[SubagentState],
  signatureVerifier: DirectoryWatchingSignatureVerifier,
  val conf: SubagentConf,
  jobLauncherConf: JobLauncherConf,
  val testEventBus: StandardEventBus[Any])
  (using ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:
  subagent =>

  protected type Termination = ProgramTermination

  if conf.scriptInjectionAllowed then logger.info("SIGNED SCRIPT INJECTION IS ALLOWED")

  val subagentRunId: SubagentRunId = SubagentRunId.fromJournalId(journal.journalId)
  private[subagent] val commandExecutor =
    new SubagentCommandExecutor(this, signatureVerifier)
  private val dedicatedAllocated =
    SetOnce[Allocated[IO, DedicatedSubagent]](SubagentNotDedicatedProblem)

  // Wollen wir SubagentCommand.DedicateDirector ???
  //private val directorRegisterable = AsyncVariable(none[DirectorRegisterable])

  private val terminated = Deferred.unsafe[IO, ProgramTermination]

  val forDirector: ForDirector = toForDirector(this)

  protected def start =
    startService:
      IO.race(
        untilStopRequested *> shutdown(processSignal = Some(SIGKILL), dontWaitForDirector = true),
        untilTerminated)
      .void

  def isShuttingDown: Boolean =
    dedicatedAllocated.toOption.fold(false)(_.allocatedThing.isShuttingDown)

  def untilTerminated: IO[ProgramTermination] =
    terminated.get

  def shutdown(
    processSignal: Option[ProcessSignal] = None,
    dontWaitForDirector: Boolean = false,
    restart: Boolean = false)
  : IO[ProgramTermination] =
    logger.debugIO(IO.defer:
      logger.info(s"❗ Shutdown ${
        Seq(processSignal, restart ? "restart", dontWaitForDirector ? "dontWaitForDirector")
          .flatten.mkString(" ")}")
      dedicatedAllocated
        .toOption
        .fold(IO.unit): allocated =>
          allocated.allocatedThing
            .terminate(processSignal, dontWaitForDirector = dontWaitForDirector)
            .guarantee(allocated.release)
        .*>(IO.defer:
          logger.info(s"$subagent stopped")
          val termination = ProgramTermination(restart = restart)
          terminated.complete(termination).attempt.as(termination)))

  def directorRegisteringResource(toRoute: DirectorRouteVariable.ToRoute): ResourceIO[Unit] =
    logger.debugResource(
      for
        _ <- directorRouteVariable.registeringRouteResource(toRoute)
        //_ <- Resource.make(
        //  acquire = registerDirector(registerable))(
        //  release = unregisterDirector)
      yield ())

  //private def registerDirector(registerable: DirectorRegisterable): IO[registerable.type] =
  //  directorRegisterable
  //    .update {
  //      case Some(_) =>
  //        IO.raiseError(new IllegalStateException(
  //          "Subagent has already registered an Director"))
  //
  //      case None =>
  //        IO.some(registerable)
  //    }
  //    .as(registerable)
  //
  //private def unregisterDirector(registerable: DirectorRegisterable): IO[Unit] =
  //  directorRegisterable
  //    .update(maybe => IO.whenA(!maybe.contains(registerable))(
  //      IO.raiseError(new IllegalStateException(
  //        "unregisterDirector tried to unregister an alien Director")))
  //      .as(None))
  //    .void

  def executeDedicateSubagent(cmd: DedicateSubagent): IO[Checked[DedicateSubagent.Response]] =
    DedicatedSubagent
      .resource(cmd.subagentId, subagentRunId, commandExecutor, journal,
        cmd.agentPath, cmd.agentRunId, cmd.controllerId, jobLauncherConf, conf)
      .toAllocated
      .flatMap: allocatedDedicatedSubagent =>
        IO:
          dedicatedAllocated.trySet(allocatedDedicatedSubagent)
        .flatMap: isFirst =>
          val ok = IO:
            logger.info(s"Subagent dedicated to be ${cmd.subagentId} in ${cmd.agentPath}, is ready")
            Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst, Some(Js7Version)))
          if isFirst then
            ok
          else
            // Maybe it's a duplicate, idempotent command?
            val errors = mutable.Buffer.empty[String]
            val existing = dedicatedAllocated.orThrow.allocatedThing
            if existing.subagentId != cmd.subagentId then
              errors += s"Renaming dedication as ${cmd.subagentId} rejected"
            if existing.agentPath != cmd.agentPath then
              errors += s"Subagent is dedicated to an other ${existing.agentPath}"
            if existing.controllerId != cmd.controllerId then
              errors += s"Subagent is dedicated to ${existing.agentPath} of alien ${existing.controllerId}"
            else if existing.agentPath == cmd.agentPath && existing.agentRunId != cmd.agentRunId then
              errors += s"Subagent is dedicated to a past or alien ${existing.agentPath}"
            if errors.isEmpty && existing.isUsed then
              errors += s"Subagent is already in use"
            if errors.nonEmpty then
              val problem = SubagentAlreadyDedicatedProblem(reasons = errors.mkString(", "))
              logger.warn(s"$cmd => $problem")
              IO.unlessA(isFirst):
                allocatedDedicatedSubagent.release
              .as(Left(problem))
            else
              ok

  def startOrderProcess(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    IO(checkedDedicatedSubagent)
      .flatMapT(_.startOrderProcess(order, executeDefaultArguments))

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Checked[Unit]] =
    subagent.checkedDedicatedSubagent
      .traverse(_
        .killProcess(orderId, signal))

  def detachProcessedOrder(orderId: OrderId): IO[Checked[Unit]] =
    IO(checkedDedicatedSubagent)
      .flatMapT(_.detachProcessedOrder(orderId))

  def releaseEvents(eventId: EventId): IO[Checked[Unit]] =
    journal.releaseEvents(eventId)

  def subagentId: Option[SubagentId] =
    checkedDedicatedSubagent.toOption.map(_.subagentId)

  def isDedicated: Boolean =
    dedicatedAllocated.isDefined

  private[subagent] def checkedDedicatedSubagent: Checked[DedicatedSubagent] =
    dedicatedAllocated.checked.map(_.allocatedThing)

  def supressJournalLogging(suppressed: Boolean): Unit =
    journal.suppressLogging(suppressed)

  override def toString = s"Subagent${checkedDedicatedSubagent.toOption.fold("")(o => s"($o)")}"

  def localUri: Uri =
    webServer.localUri


object Subagent:
  private val logger = Logger[this.type]

  def resource(conf: SubagentConf, testEventBus: StandardEventBus[Any])
    (implicit ioRuntime: IORuntime)
  : ResourceIO[Subagent] =
    import conf.config

    given Scheduler = ioRuntime.scheduler

    val alarmClockCheckingInterval = config.finiteDuration("js7.time.clock-setting-check-interval")
      .orThrow
    val useVirtualForBlocking = config.getBoolean("js7.job.execution.use-virtual-for-blocking")

    for
      actorSystem <- Pekkos.actorSystemResource(conf.name, config)
      sessionRegister <- SessionRegister.resource(SubagentSession(_), config)
      systemSessionToken <- sessionRegister
        .placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
      // Stop Subagent _after_ web service to allow Subagent to execute last commands!
      subagentDeferred <- Resource.eval(Deferred[IO, Subagent])
      directorRouteVariable = new DirectorRouteVariable
      webServer <-
        SubagentWebServer.resource(
          subagentDeferred.get, directorRouteVariable.route, sessionRegister, conf)(
          actorSystem, ioRuntime)
      _ <- provideUriFile(conf, webServer.localHttpUri)
      // For BlockingInternalJob (thread-blocking Java jobs)
      iox <- IOExecutor.resource[IO](config, conf.name + "-I/O")
      blockingInternalJobEC <-
        unlimitedExecutionContextResource[IO](
          s"${conf.name} blocking-job", conf.config, virtual = useVirtualForBlocking)
      clock <- AlarmClock.resource[IO](Some(alarmClockCheckingInterval))
      jobLauncherConf = conf.toJobLauncherConf(iox, blockingInternalJobEC, clock).orThrow
      signatureVerifier <- DirectoryWatchingSignatureVerifier.prepare(config)
        .orThrow
        .toResource(onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))(iox)
      journal <- MemoryJournal.resource(
        SubagentState.empty,
        size = config.getInt("js7.journal.memory.event-count"),
        waitingFor = "JS7 Agent Director",
        infoLogEvents = config.seqAs[String]("js7.journal.log.info-events").toSet)
      subagent <- Service.resource(IO(
        new Subagent(webServer,
          directorRouteVariable,
          ForDirector(
            _, signatureVerifier, sessionRegister, systemSessionToken, iox, testEventBus, actorSystem),
          journal, signatureVerifier,
          conf, jobLauncherConf, testEventBus)))
      _ <- Resource.eval(subagentDeferred.complete(subagent))
    yield
      logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
      subagent

  //def blockingInternalJobEC(name: String, config: Config, virtual: Boolean)
  //: ResourceIO[ExecutionContext] =
  //  unlimitedExecutionContextResource[IO](
  //    s"$name blocking-job", config, virtual = virtual)

  private def provideUriFile(conf: SubagentConf, uri: Checked[Uri]): ResourceIO[Path] =
    provideFile[IO](conf.workDirectory / "http-uri")
      .evalTap(file => IO {
        for uri <- uri do file := s"$uri/subagent"
      })

  final case class ForDirector(
    subagent: Subagent,
    signatureVerifier: DirectoryWatchingSignatureVerifier,
    sessionRegister: SessionRegister[SubagentSession],
    systemSessionToken: SessionToken,
    ioExecutor: IOExecutor,
    testEventBus: StandardEventBus[Any],
    actorSystem: ActorSystem):
    given iox: IOExecutor = ioExecutor

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
