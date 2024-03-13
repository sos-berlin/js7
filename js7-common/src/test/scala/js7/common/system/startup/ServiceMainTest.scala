package js7.common.system.startup

import cats.effect.{ExitCode, IO}
import com.typesafe.config.Config
import js7.base.service.Service.StoppableByRequest
import js7.base.service.{MainService, MainServiceTerminationException, Service}
import js7.base.system.startup.Js7ReturnCodes
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ProgramTermination
import js7.common.configuration.{BasicConfiguration, Js7Configuration}
import js7.common.system.startup.ServiceMainTest.*

final class ServiceMainTest extends OurAsyncTestSuite:

  "Normal run" in:
    val conf = TestConf(Js7Configuration.defaultConfig)
    for
      exitCode <- ServiceMain.runAsMain(Nil, "TEST", _ => conf)(
        toServiceResource = _ => TestService.resource)
    yield
      assert(exitCode == ExitCode(0))

  "MainServiceTerminationException is properly handeled" in:
    val conf = TestConf(Js7Configuration.defaultConfig)
    for
      exitCode <- ServiceMain.runAsMain(Nil, "TEST", _ => conf)(
        toServiceResource = _ => TerminatingService.resource)
    yield
      assert(exitCode == ExitCode(Js7ReturnCodes.Restart))


object ServiceMainTest:
  private case class TestConf(config: Config) extends BasicConfiguration

  private class TestService extends MainService, StoppableByRequest:
    protected type Termination = ProgramTermination

    protected def start =
      startService(untilStopRequested)

    def untilTerminated =
      IO.pure(ProgramTermination())

  private object TestService:
    val resource = Service.resource(IO(new TestService))


  private val termination = ProgramTermination.Restart


  private class TerminatingService extends MainService, StoppableByRequest:
    protected type Termination = ProgramTermination

    protected def start =
      startService(IO.raiseError(new MainServiceTerminationException {
        def termination = ServiceMainTest.termination
        override def getMessage = "Test termination!"
      }))

    def untilTerminated =
      IO.pure(ProgramTermination())

  private object TerminatingService:
    val resource = Service.resource(IO(new TerminatingService))
