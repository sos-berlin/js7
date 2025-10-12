package js7.common.system.startup

import cats.effect.{ExitCode, IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.config.Js7Config
import js7.base.service.Service.StoppableByRequest
import js7.base.service.{MainService, MainServiceTerminationException, Service}
import js7.base.system.startup.Js7ReturnCodes
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ProgramTermination
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceMainTest.*

final class ServiceMainTest extends OurAsyncTestSuite:

  "Normal run" in:
    val conf = TestConf(Js7Config.defaultConfig)
    for
      exitCode <- ServiceMain.runAsMain(Nil, "TEST", _ => conf): _ =>
        TestService.service
    yield
      assert(exitCode == ExitCode(77))

  "MainServiceTerminationException is properly handled" in:
    val conf = TestConf(Js7Config.defaultConfig)
    for
      exitCode <- ServiceMain.runAsMain(Nil, "TEST", _ => conf)(_ => TerminatingService.service)
    yield
      assert(exitCode == ExitCode(Js7ReturnCodes.Restart))


object ServiceMainTest:
  private case class TestConf(config: Config) extends BasicConfiguration:
    val name = "ServiceMainTest"

  private class TestService extends MainService, Service.Trivial:
    protected type Termination = ProgramTermination

    def untilTerminated =
      IO.pure(ProgramTermination.fromExitCode(ExitCode(77)))

  private object TestService:
    val service: ResourceIO[TestService] =
      Service.resource(new TestService)


  private val termination = ProgramTermination.Restart


  private class TerminatingService extends MainService, StoppableByRequest:
    protected type Termination = ProgramTermination

    protected def start =
      startService:
        IO.raiseError:
          new MainServiceTerminationException:
            def termination = ServiceMainTest.termination
            override def getMessage = "Test termination!"

    def untilTerminated =
      IO.pure(ProgramTermination())

  private object TerminatingService:
    val service: ResourceIO[TerminatingService] =
      Service.resource(new TerminatingService)
