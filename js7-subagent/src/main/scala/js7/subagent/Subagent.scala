package js7.subagent

import akka.actor.ActorSystem
import cats.effect.Resource
import cats.effect.concurrent.Deferred
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
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.RichMonixResource
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.{MainService, Service}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.base.web.Uri
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.SessionRegister
import js7.common.akkautils.Akkas
import js7.common.system.ThreadPools.unlimitedSchedulerResource
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
import monix.eval.{Fiber, Task}
import monix.execution.Scheduler

final class Subagent private(
  webServer: AkkaWebServer,
  directorRouteVariable: DirectorRouteVariable,
  toForDirector: Subagent => ForDirector,
  val journal: MemoryJournal[SubagentState],
  signatureVerifier: DirectoryWatchingSignatureVerifier,
  val conf: SubagentConf,
  jobLauncherConf: JobLauncherConf,
  val testEventBus: StandardEventBus[Any])
extends MainService with Service.StoppableByRequest
{
  subagent =>

  if (conf.scriptInjectionAllowed) logger.info("SIGNED SCRIPT INJECTION IS ALLOWED")

  val subagentRunId = SubagentRunId.fromJournalId(journal.journalId)
  private[subagent] val commandExecutor =
    new SubagentCommandExecutor(this, signatureVerifier)
  private val dedicatedAllocated =
    SetOnce[Allocated[Task, DedicatedSubagent]](SubagentNotDedicatedProblem)

  // Wollen wir SubagentCommand.DedicateDirector ???
  //private val directorRegisterable = AsyncVariable(none[DirectorRegisterable])

  private val terminated = Deferred.unsafe[Task, ProgramTermination]

  val forDirector: ForDirector = toForDirector(this)

  protected def start =
    startService(Task
      .race(
        untilStopRequested *> shutdown(processSignal = None),
        untilTerminated)
      .void)

  def isShuttingDown: Boolean =
    dedicatedAllocated.toOption.fold(false)(_.allocatedThing.isShuttingDown)

  def untilTerminated: Task[ProgramTermination] =
    terminated.get

  def shutdown(
    processSignal: Option[ProcessSignal] = None,
    restart: Boolean = false,
    dontWaitForDirector: Boolean = false)
  : Task[ProgramTermination] =
    logger.debugTask(Task.defer {
      logger.info(s"❗ Shutdown ${
        Seq(processSignal, restart ? "restart", dontWaitForDirector ? "dontWaitForDirector")
          .flatten.mkString(" ")}")
      dedicatedAllocated
        .toOption
        .fold(Task.unit)(allocated =>
          allocated.allocatedThing
            .terminate(processSignal, dontWaitForDirector = dontWaitForDirector)
            .guarantee(allocated.release))
        .*>(Task.defer {
          logger.info(s"$subagent stopped")
          val termination = ProgramTermination(restart = restart)
          terminated.complete(termination).attempt.as(termination)
        })
    })

  def directorRegisteringResource(toRoute: DirectorRouteVariable.ToRoute): Resource[Task, Unit] =
    logger.debugResource(
      for {
        _ <- directorRouteVariable.registeringRouteResource(toRoute)
        //_ <- Resource.make(
        //  acquire = registerDirector(registerable))(
        //  release = unregisterDirector)
      } yield ())

  //def directorRegisteringResource(registerable: DirectorRegisterable): Resource[Task, Unit] =
  //  for {
  //    _ <- webServer.registeringRouteResource(registerable.toRoute)
  //    //_ <- Resource.make(
  //    //  acquire = registerDirector(registerable))(
  //    //  release = unregisterDirector)
  //  } yield ()

  //private def registerDirector(registerable: DirectorRegisterable): Task[registerable.type] =
  //  directorRegisterable
  //    .update {
  //      case Some(_) =>
  //        Task.raiseError(new IllegalStateException(
  //          "Subagent has already registered an Director"))
  //
  //      case None =>
  //        Task.some(registerable)
  //    }
  //    .as(registerable)
  //
  //private def unregisterDirector(registerable: DirectorRegisterable): Task[Unit] =
  //  directorRegisterable
  //    .update(maybe => Task.when(!maybe.contains(registerable))(
  //      Task.raiseError(new IllegalStateException(
  //        "unregisterDirector tried to unregister an alien Director")))
  //      .as(None))
  //    .void

  def executeDedicateSubagent(cmd: DedicateSubagent): Task[Checked[DedicateSubagent.Response]] =
    DedicatedSubagent
      .resource(cmd.subagentId, subagentRunId, commandExecutor, journal,
        cmd.agentPath, cmd.controllerId, jobLauncherConf, conf)
      .toAllocated
      .flatMap(allocatedDedicatedSubagent => Task.defer {
        val isFirst = dedicatedAllocated.trySet(allocatedDedicatedSubagent)
        if (!isFirst) {
          // TODO Idempotent: Frisch gewidmeter Subagent ist okay. Kein Kommando darf eingekommen sein.
          //if (cmd.subagentId == dedicatedAllocated.orThrow.subagentId)
          //  Task.pure(Right(DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst)))
          //else
          logger.warn(s"$cmd => $SubagentAlreadyDedicatedProblem: $dedicatedAllocated")
          Task.left(SubagentAlreadyDedicatedProblem)
        } else {
          // TODO Check agentPath, controllerId (handle in SubagentState?)
          logger.info(s"Subagent dedicated to be ${cmd.subagentId} in ${cmd.agentPath}, is ready")
          Task.right(
            DedicateSubagent.Response(subagentRunId, EventId.BeforeFirst, Some(Js7Version)))
        }
      })

  def startOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Fiber[OrderProcessed]]] =
    Task(checkedDedicatedSubagent)
      .flatMapT(_.startOrderProcess(order, defaultArguments))

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Checked[Unit]] =
    subagent.checkedDedicatedSubagent
      .traverse(_
        .killProcess(orderId, signal))

  def detachProcessedOrder(orderId: OrderId): Task[Checked[Unit]] =
    Task(checkedDedicatedSubagent)
      .flatMapT(_.detachProcessedOrder(orderId))

  def releaseEvents(eventId: EventId): Task[Checked[Unit]] =
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
}

object Subagent
{
  private val logger = Logger(getClass)

  def resource(conf: SubagentConf, testEventBus: StandardEventBus[Any])
    (implicit scheduler: Scheduler)
  : Resource[Task, Subagent] = {
    import conf.config

    val alarmClockCheckingInterval = config.finiteDuration("js7.time.clock-setting-check-interval")
      .orThrow

    for {
      // Stop Subagent _after_ web service to allow Subagent to execute last commands!
      subagentDeferred <- Resource.eval(Deferred[Task, Subagent])
      directorRouteVariable = new DirectorRouteVariable
      actorSystem <- Akkas.actorSystemResource(conf.name, config)
      sessionRegister <- {
        implicit val a = actorSystem
        SessionRegister.resource(SubagentSession(_), config)
      }
      systemSessionToken <- sessionRegister
        .placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
      webServer <-
        SubagentWebServer.resource(
          subagentDeferred.get, directorRouteVariable.route, sessionRegister, conf)(
          actorSystem, scheduler)
      _ <- provideUriFile(conf, webServer.localHttpUri)
      // For BlockingInternalJob (thread-blocking Java jobs)
      blockingInternalJobScheduler <- unlimitedSchedulerResource[Task](
        "JS7 blocking job", conf.config)
      clock <- AlarmClock.resource[Task](Some(alarmClockCheckingInterval))
      journal <- MemoryJournal.resource(
        SubagentState.empty,
        size = config.getInt("js7.journal.in-memory.event-count"),
        waitingFor = "JS7 Agent Director",
        infoLogEvents = config.seqAs[String]("js7.journal.log.info-events").toSet)
      iox <- IOExecutor.resource[Task](config, conf.name + "-I/O")
      jobLauncherConf = conf.toJobLauncherConf(iox, blockingInternalJobScheduler, clock).orThrow
      signatureVerifier <- DirectoryWatchingSignatureVerifier.prepare(config)
        .orThrow
        .toResource(onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))(iox)
      subagent <- Service.resource(Task(
        new Subagent(webServer,
          directorRouteVariable,
          ForDirector(
            _, signatureVerifier, sessionRegister, systemSessionToken, iox, testEventBus, actorSystem),
          journal, signatureVerifier,
          conf, jobLauncherConf, testEventBus)))
      _ <- Resource.eval(subagentDeferred.complete(subagent))
    } yield {
      logger.info("Subagent is ready to be dedicated" + "\n" + "─" * 80)
      subagent
    }
  }.executeOn(scheduler)

  private def provideUriFile(conf: SubagentConf, uri: Checked[Uri]): Resource[Task, Path] =
    provideFile[Task](conf.workDirectory / "http-uri")
      .evalTap(file => Task {
        for (uri <- uri) file := s"$uri/subagent"
      })

  final case class ForDirector(
    subagent: Subagent,
    signatureVerifier: DirectoryWatchingSignatureVerifier,
    sessionRegister: SessionRegister[SubagentSession],
    systemSessionToken: SessionToken,
    implicit val iox: IOExecutor,
    testEventBus: StandardEventBus[Any],
    actorSystem: ActorSystem)

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
}
