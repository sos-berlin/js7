package js7.tests.controller.proxy

import java.util.Arrays.asList
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.tests.testenv.ControllerAgentForScalaTest

final class JSubagentTest extends OurTestSuite with ControllerAgentForScalaTest:
  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXY-PASSWORD"
        permissions = [ UpdateItem ]
      }
    }
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = AgentPath("AGENT") :: Nil
  protected val items = Nil

  "Test" in:
    JSubagentTester.run(asList(
      JAdmission(
        Admission(
          controller.localUri,
          Some(
            UserAndPassword(UserId("Proxy"),
              SecretString("PROXY-PASSWORD")))))),
      JHttpsConfig.empty)
