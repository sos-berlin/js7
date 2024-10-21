package js7.agent

import cats.effect.Resource.ExitCase
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO, Sync, kernel}
import cats.syntax.flatMap.*
import js7.agent.RunningAgent.TestWiring
import js7.agent.TestAgent.*
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.auth.SessionToken
import js7.base.catsutils.{OurIORuntime, OurIORuntimeRegister}
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.tapError
import js7.base.problem.Checked
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.common.system.startup.ServiceMain
import js7.core.command.CommandMeta
import js7.journal.watch.EventWatch
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

final class TestAgent(
  allocated: Allocated[IO, RunningAgent],
  terminateProcessesWith: Option[ProcessSignal] = None):

  val agent = allocated.allocatedThing
  @volatile private var released = false

  def stop: IO[Unit] =
    agent.terminate(terminateProcessesWith).void *>
      logger.traceIO("allocated.release"):
        allocated.release *> IO:
          released = true

  def killForFailOver: IO[ProgramTermination] =
    terminate(
      processSignal = Some(SIGTERM),
      clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))

  def terminate(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false)
  : IO[ProgramTermination] =
    IO.defer:
      if released then
        // The Agent's own test IORuntime have have been shutdown
        IO.pure(ProgramTermination())
      else
        logger.traceIO:
          agent
            .terminate(processSignal, clusterAction, suppressSnapshot)
            .guarantee(stop)

  def untilTerminated: IO[ProgramTermination] =
    agent.untilTerminated <*
      stop/*release outer resources*/

  /** Use TestAgent once, then stop it. */
  def useSync[R](timeout: FiniteDuration)(body: TestAgent => R)(using IORuntime): R =
    logger.debugCall[R](s"useSync »${agent.conf.name}«")(
      allocated.useSync(timeout)(_ => body(this)))

  implicit def actorSystem: ActorSystem =
    agent.actorSystem

  def localUri: Uri =
    agent.localUri

  def sessionToken: SessionToken =
    agent.systemSessionToken

  def untilReady: IO[MainActor.Ready] =
    agent.untilReady

  def currentAgentState(): AgentState =
    given IORuntime = agent.ioRuntime
    agent.agentState.await(99.s).orThrow

  def eventWatch: EventWatch =
    agent.eventWatch

  def testEventBus: StandardEventBus[Any] =
    agent.testEventBus

  def executeCommandAsSystemUser(command: AgentCommand): IO[Checked[AgentCommand.Response]] =
    agent.executeCommandAsSystemUser(command)

  def executeCommand(cmd: AgentCommand, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    agent.executeCommand(cmd: AgentCommand, meta)

  def name: String =
    conf.name

  def conf: AgentConfiguration =
    agent.conf

  override def toString = s"TestAgent($name)"


object TestAgent:
  private val logger = Logger[this.type]

  def apply(
    allocated: Allocated[IO, RunningAgent],
    terminateProcessesWith: Option[ProcessSignal] = None)
  : TestAgent =
    new TestAgent(allocated, terminateProcessesWith)

  def start(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty,
    terminateProcessesWith: Option[ProcessSignal] = None)
  : IO[TestAgent] =
    CorrelId.bindNew:
      ioRuntimeResource[IO](conf)
        .flatTap: ioRuntime =>
          OurIORuntimeRegister.toEnvironment(ioRuntime)
            .registerMultiple(testWiring.envResources)
        .flatMap: ioRuntime =>
          given IORuntime = ioRuntime
          RunningAgent.resource(conf, testWiring).evalOn(ioRuntime.compute)
        .toAllocated
        .map(new TestAgent(_, terminateProcessesWith))

  def blockingRun(
    conf: AgentConfiguration,
    timeout: FiniteDuration = 99.s,
    terminateProcessesWith: Option[ProcessSignal] = None)
    (body: TestAgent => Unit)
    (using ioRuntime: IORuntime)
  : ProgramTermination =
    ServiceMain.blockingRun(timeout = timeout)(
      resource = resource(conf).evalOn(ioRuntime.compute),
      use = (agent: RunningAgent) =>
        try
          val testAgent = new TestAgent(new Allocated(agent, agent.terminate().void))
          try body(testAgent)
          catch { case NonFatal(t) =>
            logger.debug(s"💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
            try agent.terminate(terminateProcessesWith).await(99.s)
            catch case NonFatal(t2) => logger.error(
              s"agent.terminate while handling an exception: ${t.toStringWithCauses}",
              t.nullIfNoStackTrace)
            throw t
          }
          agent.terminate(terminateProcessesWith).await(99.s)
        catch case NonFatal(t) =>
          // Silent deadlock in case of failure?
          logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          throw t)

  def resource(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningAgent] =
    RunningAgent.resource(conf, testWiring)
      .flatMap: agent =>
        Resource.makeCase(
          acquire = IO.pure(agent))(
          release = (agent, exitCase) => IO.defer:
            exitCase match
              case ExitCase.Errored(throwable) =>
                logger.error(throwable.toStringWithCauses, throwable.nullIfNoStackTrace)
              case _ =>
            // Avoid Akka 2.6 StackTraceError which occurs when
            // agent.terminate() has not been executed:
            agent.untilTerminated.void
              .timeoutTo(3.s, IO.unit)
              .tapError: t =>
                IO(logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)))


  private def ioRuntimeResource[F[_]](conf: AgentConfiguration)(implicit F: Sync[F])
  : Resource[F, IORuntime] =
      OurIORuntime.resource[F](conf.name, conf.config)
