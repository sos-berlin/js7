package js7.common.system.startup

import com.typesafe.config.Config
import js7.base.io.process.ReturnCode
import js7.base.service.Service.StoppableByRequest
import js7.base.service.{MainService, MainServiceTerminationException, Service}
import js7.base.test.OurTestSuite
import js7.base.utils.ProgramTermination
import js7.common.configuration.{BasicConfiguration, Js7Configuration}
import js7.common.system.startup.ServiceMainTest.*
import monix.eval.Task

final class ServiceMainTest extends OurTestSuite:

  "Normal run" in:
    val conf = TestConf(Js7Configuration.defaultConfig)
    val returnCode = ServiceMain.returnCodeMain(Array.empty, "TEST", _ => conf)(
      (_, _) => TestService.resource)
    assert(returnCode == ReturnCode(0))

  "MainServiceTerminationException" in:
    val conf = TestConf(Js7Configuration.defaultConfig)
    val returnCode = ServiceMain.returnCodeMain(Array.empty, "TEST", _ => conf)(
      (_, _) => TerminatingService.resource)
    assert(returnCode == ReturnCode(Js7ReturnCodes.Restart))


object ServiceMainTest:
  private case class TestConf(config: Config) extends BasicConfiguration

  private class TestService extends MainService, StoppableByRequest:
    protected type Termination = ProgramTermination

    protected def start =
      startService(untilStopRequested)

    def untilTerminated =
      Task.pure(ProgramTermination())
  private object TestService:
    val resource = Service.resource(Task(new TestService))

  private val termination = ProgramTermination(restart = true)

  private class TerminatingService extends MainService, StoppableByRequest:
    protected type Termination = ProgramTermination

    protected def start =
      startService(Task.raiseError(new MainServiceTerminationException {
        def termination = ServiceMainTest.termination
        override def getMessage = "Test termination!"
      }))

    def untilTerminated =
      Task.pure(ProgramTermination())
  private object TerminatingService:
    val resource = Service.resource(Task(new TerminatingService))
