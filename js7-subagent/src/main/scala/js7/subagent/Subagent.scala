package js7.subagent

import cats.effect.std.Supervisor
import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.traverse.*
import java.nio.file.Path
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.catsutils.Environment.{TaggedResource, environment}
import js7.base.catsutils.{Environment, OurIORuntimeRegister}
import js7.base.configutils.Configs.RichConfig
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.log4j.Log4j
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.{MainService, Service}
import js7.base.stream.Numbered
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.thread.IOExecutor
import js7.base.time.{AlarmClock, Timestamp, WallClock}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.flattenToString
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Missing, ProgramTermination, ScalaUtils, SetOnce}
import js7.base.web.Uri
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.session.SessionRegister
import js7.common.pekkoutils.Pekkos
import js7.common.system.ThreadPools.unlimitedExecutionContextResource
import js7.core.command.CommandMeta
import js7.data.event.EventId
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.Problems.{SubagentAlreadyDedicatedProblem, SubagentNotDedicatedProblem}
import js7.data.subagent.SubagentCommand.DedicateSubagent
import js7.data.subagent.{SubagentCommand, SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.journal.MemoryJournal
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.crashpidfile.{CrashPidFile, CrashPidFileService}
import js7.launcher.process.PipedProcess
import js7.subagent.Subagent.*
import js7.subagent.configuration.SubagentConf
import js7.subagent.web.SubagentWebServer
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable

final class Subagent private(
  val webServer: PekkoWebServer,
  directorRouteVariable: DirectorRouteVariable,
  toForDirector: Subagent => ForDirector,
  val journal: MemoryJournal[SubagentState],
  signatureVerifier: DirectoryWatchingSignatureVerifier,
  val conf: SubagentConf,
  jobLauncherConf: JobLauncherConf,
  val testEventBus: StandardEventBus[Any],
  supervisor: Supervisor[IO])
  (using ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:
  subagent =>

  protected type Termination = ProgramTermination

  if conf.scriptInjectionAllowed then logger.info("SIGNED SCRIPT INJECTION IS ALLOWED")

  val subagentRunId: SubagentRunId = SubagentRunId.fromJournalId(journal.journalId)
  private[subagent] val commandExecutor =
    SubagentCommandExecutor(this, signatureVerifier, supervisor)
  private val dedicatedAllocated =
    SetOnce[Allocated[IO, DedicatedSubagent]](SubagentNotDedicatedProblem)

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
    logger.debugIO:
      terminated.tryGet.flatMap:
        _.map(IO.pure).getOrElse:
          IO.defer:
            logger.info(s"❗ Shutdown ${
              flattenToString(
                processSignal, restart ? "restart", dontWaitForDirector ? "dontWaitForDirector")}")
            dedicatedAllocated
              .toOption
              .fold(IO.unit): allocated =>
                allocated.allocatedThing
                  .stop(processSignal, dontWaitForDirector = dontWaitForDirector)
                  .guarantee(allocated.release)
              .*>(IO.defer:
                val termination = ProgramTermination(restart = restart)
                terminated.complete(termination).attempt.as(termination))

  def registerDirectorRoute(toRoute: DirectorRouteVariable.ToRoute): ResourceIO[Unit] =
    logger.debugResource:
      directorRouteVariable.registeringRouteResource(toRoute)

  @TestOnly
  def executeCommandForTest(cmd: SubagentCommand.Queueable): IO[Checked[SubagentCommand.Response]] =
    executeCommand(cmd)

  private[subagent] def executeCommand(cmd: SubagentCommand.Queueable)
  : IO[Checked[SubagentCommand.Response]] =
    commandExecutor.executeCommand(Numbered(0, cmd), CommandMeta.System)

  def executeDedicateSubagent(cmd: DedicateSubagent): IO[Checked[DedicateSubagent.Response]] =
    DedicatedSubagent
      .service(cmd.subagentId, subagentRunId, commandExecutor, journal,
        cmd.agentPath, cmd.agentRunId, cmd.controllerId, jobLauncherConf, conf)
      .toAllocated
      .flatMap: allocatedDedicatedSubagent =>
        IO:
          dedicatedAllocated.trySet(allocatedDedicatedSubagent)
        .flatMap: isFirst =>
          if isFirst then
            IO.right(())
          else
            handleDuplicateDedication(cmd, dedicatedAllocated.orThrow.allocatedThing) match
              case Left(problem) =>
                IO.defer:
                  logger.warn(s"$cmd => $problem")
                  allocatedDedicatedSubagent.release.as(Left(problem))
              case Right(()) =>
                IO.right(())
        .flatMapT: _ =>
          IO:
            Log4j.set("js7.serverId", cmd.subagentId.toString)
            logger.info:
              s"Subagent dedicated as ${cmd.subagentId} to ${cmd.agentPath}, is ready"
            Right:
              DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst, Js7Version)

  /** Maybe the duplicate command is idempotent? */
  private def handleDuplicateDedication(cmd: DedicateSubagent, existing: DedicatedSubagent)
  : Checked[Unit] =
    val errors = mutable.Buffer.empty[String]
    if existing.subagentId != cmd.subagentId then
      errors += s"Renaming dedication as ${cmd.subagentId} rejected"
    if existing.agentPath != cmd.agentPath then
      errors += s"Subagent is already dedicated to ${existing.agentPath}"
    if existing.controllerId != cmd.controllerId then
      errors += s"Subagent is dedicated to ${existing.agentPath} of alien ${existing.controllerId}"
    else if existing.agentPath == cmd.agentPath && existing.agentRunId != cmd.agentRunId then
      errors += s"Subagent is dedicated to a past or alien ${existing.agentPath}"
    if errors.isEmpty && existing.isUsed then
      errors += s"Subagent is already in use"
    errors.isEmpty !!
      SubagentAlreadyDedicatedProblem(reasons = errors.mkString(", "))

  def startOrderProcess(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    IO(checkedDedicatedSubagent)
      .flatMapT(_.startOrderProcess(order, executeDefaultArguments, endOfAdmissionPeriod))

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Checked[Unit]] =
    subagent.checkedDedicatedSubagent.traverse:
      _.killProcess(orderId, signal)

  def releaseEvents(eventId: EventId): IO[Checked[Unit]] =
    IO(checkedDedicatedSubagent).flatMapT:
      _.releaseEvents(eventId)

  def subagentId: Option[SubagentId] =
    checkedDedicatedSubagent.toOption.map(_.subagentId)

  def isDedicated: Boolean =
    dedicatedAllocated.isDefined

  private[subagent] def checkedDedicatedSubagent: Checked[DedicatedSubagent] =
    dedicatedAllocated.checked.map(_.allocatedThing)

  def suppressJournalLogging(suppressed: Boolean): Unit =
    journal.suppressLogging(suppressed)

  override def toString =
    s"Subagent${dedicatedAllocated.toOption.map(_.allocatedThing.longName).fold("")(o => s"($o)")}"

  def localUri: Uri =
    webServer.localUri


object Subagent:
  private val logger = Logger[this.type]

  def service(conf: SubagentConf, testWiring: TestWiring)
    (using ioRuntime: IORuntime)
  : ResourceIO[Subagent] =
    import conf.config

    given Scheduler = ioRuntime.scheduler

    val alarmClockCheckingInterval = config.finiteDuration("js7.time.clock-setting-check-interval")
      .orThrow
    val useVirtualForBlocking = config.getBoolean("js7.job.execution.use-virtual-for-blocking-job")

    logger.debugResource:
      import testWiring.testEventBus
      for
        _ <- OurIORuntimeRegister.toEnvironment(ioRuntime).registerMultiple(testWiring.envResources)
        _ <- registerStaticMBean("Process", PipedProcess.ProcessMXBean)
        pidFile <- CrashPidFileService.file(CrashPidFile.dataDirToFile(conf.dataDirectory))
        actorSystem <- Pekkos.actorSystemResource(conf.name, config)
        sessionRegister <- SessionRegister.service(SubagentSession(_), config)
        systemSessionToken <- sessionRegister
          .placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
        // Stop Subagent _after_ web service to allow Subagent to execute last commands!
        subagentDeferred <- Resource.eval(Deferred[IO, Subagent])
        directorRouteVariable = new DirectorRouteVariable
        webServer <- SubagentWebServer.service(
          subagentDeferred.get, directorRouteVariable.route, sessionRegister, conf)
          (using actorSystem, ioRuntime)
        _ <- provideUriFile(conf, webServer.localHttpUri)
        // For BlockingInternalJob (thread-blocking Java jobs)
        iox <- Resource.eval(environment[IOExecutor])
        blockingInternalJobEC <-
          unlimitedExecutionContextResource[IO](
            s"${conf.name} blocking-job", virtual = useVirtualForBlocking)
        clock <- Environment.getOrRegister[AlarmClock]:
          AlarmClock.resource[IO](Some(alarmClockCheckingInterval))
        jobLauncherConf = conf.toJobLauncherConf(iox, blockingInternalJobEC, clock, pidFile).orThrow
        signatureVerifier <- DirectoryWatchingSignatureVerifier.Provider(clock, config)
          .prepare.orThrow
          .toResource(onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))
        journal <- MemoryJournal.resource(
          SubagentState.empty,
          size = config.getInt("js7.journal.memory.event-count"),
          waitingFor = "JS7 Agent Director",
          infoLogEvents = config.seqAs[String]("js7.journal.log.info-events").toSet)
        supervisor <- Supervisor[IO]
        subagent <- Service.resource:
          new Subagent(
            webServer, directorRouteVariable,
            ForDirector(
              _, signatureVerifier, sessionRegister, systemSessionToken, testEventBus, actorSystem),
            journal, signatureVerifier,
            conf, jobLauncherConf, testEventBus,
            supervisor)
        _ <- subagentDeferred.complete(subagent).toResource
      yield
        logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
        subagent

  private def provideUriFile(conf: SubagentConf, uri: Checked[Uri]): ResourceIO[Path] =
    provideFile[IO](conf.workDirectory / "http-uri")
      .evalTap: file =>
        IO.blocking:
          for uri <- uri do file := s"$uri/subagent"


  /** Some objects the local Subagent provides for the Director. */
  final case class ForDirector(
    subagent: Subagent,
    signatureVerifier: DirectoryWatchingSignatureVerifier,
    sessionRegister: SessionRegister[SubagentSession],
    systemSessionToken: SessionToken,
    testEventBus: StandardEventBus[Any],
    actorSystem: ActorSystem)


  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated


  final case class TestWiring private(
    testEventBus: StandardEventBus[Any],
    envResources: Seq[Environment.TaggedResource[IO, ?]])

  object TestWiring:
    val empty: TestWiring = TestWiring()

    def apply(
      testEventBus: StandardEventBus[Any] = StandardEventBus(),
      envResources: Seq[Environment.TaggedResource[IO, ?]] = Nil,
      clock: AlarmClock | Missing = Missing)
    : TestWiring =
      new TestWiring(
        testEventBus,
        envResources ++
          clock.toOption.toSeq.flatMap: alarmClock =>
            Seq(
              TaggedResource.eval(IO.pure[AlarmClock](alarmClock)),
              TaggedResource.eval(IO.pure[WallClock](alarmClock))))
