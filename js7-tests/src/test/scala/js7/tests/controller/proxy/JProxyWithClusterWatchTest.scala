package js7.tests.controller.proxy

import js7.base.test.OurTestSuite
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.jdk.CollectionConverters.*

final class JProxyWithClusterWatchTest extends OurTestSuite with ControllerAgentForScalaTest
{
  protected def items = Nil
  protected def agentPaths = Nil

  "test" in {
    JProxyWithClusterWatchTester.test(
      List(JAdmission(controllerAdmission)).asJava,
      JHttpsConfig.empty)
  }
}
