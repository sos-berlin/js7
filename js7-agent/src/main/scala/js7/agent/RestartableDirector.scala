package js7.agent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.flatMap.*
import js7.agent.RestartableDirector.*
import js7.agent.RunningAgent.TestWiring
import js7.agent.configuration.AgentConfiguration
import js7.base.catsutils.CatsEffectExtensions.{right, startAndLogError}
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.{dematerialize, tapError}
import js7.base.monixutils.AsyncVariable
import js7.base.service.{MainService, Service}
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Nulls.nullToNone
import js7.common.pekkohttp.web.PekkoWebServer
import js7.subagent.Subagent
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success, Try}

final class RestartableDirector private(
  subagent: Subagent,
  conf: AgentConfiguration,
  testWiring: TestWiring)
  (using ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:

  protected type Termination = DirectorTermination

  private val _currentDirector = AsyncVariable[RunningAgent | Null](null)
  private val _untilTerminated = Deferred.unsafe[IO, Try[DirectorTermination]]

  protected def start =
    startService:
      loop.flatMap: termination =>
        _untilTerminated.complete(Success(termination)).void
      .tapError: t => // TODO Shouldn't MainService handle this ?
        _untilTerminated.complete(Failure(t)).void

  private def loop: IO[DirectorTermination] =
    ().tailRecM: _ =>
      val t = Deadline.now
      runDirector.flatMap: termination =>
        if termination.restartDirector then
          logger.info(s"⟳ Restart Agent Director\n" + "─" * 80)
          IO.sleep((t + MinimumRestartDuration).timeLeft).as(Left(()))
        else
          IO.right(termination)

  private def runDirector: IO[DirectorTermination] =
    RunningAgent.service(subagent, conf, testWiring).use: director =>
      _currentDirector.set(director) *>
        onStopRequested(director.terminate().void).surround:
          director.untilTerminated
        .guarantee:
          _currentDirector.set(null)

  private def onStopRequested(stop: IO[Unit]): ResourceIO[Unit] =
    Resource
      .make(
        acquire = (untilStopRequested *> stop).startAndLogError)(
        release = _.cancel)
      .void

  def untilTerminated: IO[DirectorTermination] =
    _untilTerminated.get.dematerialize

  @TestOnly
  def currentDirector: IO[RunningAgent] =
    _currentDirector.value.map(nullToNone).flatMap:
      case None =>
        IO.raiseError(new IllegalStateException("RestartableDirector.currentDirector: Not started"))
      case Some(o) => IO.pure(o)

  def webServer: PekkoWebServer =
    subagent.webServer

  override def toString = "RestartableDirector"


private object RestartableDirector:
  private val logger = Logger[this.type]
  private val MinimumRestartDuration = 10.s

  def service(
    subagent: Subagent,
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (using IORuntime)
  : ResourceIO[RestartableDirector] =
    Service.resource:
      new RestartableDirector(subagent, conf, testWiring)
