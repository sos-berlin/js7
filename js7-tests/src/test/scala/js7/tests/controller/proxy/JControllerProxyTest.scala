package js7.tests.controller.proxy

import io.circe.syntax._
import java.nio.file.Files.createDirectory
import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.utils.Lazy
import js7.base.web.Uri
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.controller.ControllerFileBaseds.jsonCodec
import js7.data.filebased.{FileBased, VersionId}
import js7.data.job.ExecutablePath
import js7.proxy.javaapi.JAdmission
import js7.proxy.javaapi.data.JHttpsConfig
import js7.tests.controller.proxy.JControllerProxyTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class JControllerProxyTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateRepo ]
      }
    }
    """

  protected val fileBased = Nil
  protected val agentRefPaths = JournaledProxyTest.agentRefPath :: Nil

  override def beforeAll() = {
    super.beforeAll()
    (directoryProvider.controller.configDir / "private" / "private.conf") ++= s"""
      |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
      |""".stripMargin

    for (configDir <- List(directoryProvider.controller.configDir, directoryProvider.agents(0).configDir)) {
      (configDir / "private" / "private.conf") ++= s"""
        |js7.configuration.trusted-signature-keys.Silly = $${js7.config-directory}"/private/trusted-silly-signature-keys"
        |""".stripMargin
      createDirectory(configDir / "private/trusted-silly-signature-keys")
      configDir / "private/trusted-silly-signature-keys/key.silly" := "MY-SILLY-SIGNATURE"
    }

    directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), script(1.s))
  }

  "JControllerProxy" in {
    directoryProvider.runAgents() { _ =>
      val port = findFreeTcpPort()
      val controller = Lazy { directoryProvider.startController(httpPort = Some(port)).await(99.s) }
      try {
        val admissions = List(JAdmission.of(s"http://127.0.0.1:$port", JournaledProxyTest.primaryCredentials)).asJava
        JControllerProxyTester.run(admissions, JHttpsConfig.empty,
          (JournaledProxyTest.workflow.withVersion(VersionId("MY-VERSION")): FileBased).asJson.compactPrint,
          (unusedAgentRef.withVersion(VersionId("MY-VERSION")): FileBased).asJson.compactPrint,
          () => controller())
      } finally
        for (controller <- controller) {
          controller.terminate() await 99.s
          controller.close()
        }
    }
  }
}

object JControllerProxyTest
{
  private val unusedAgentRef = AgentRef(AgentRefPath("/ADDED-AGENT") ~ "INITIAL", Uri("http://localhost:1"))
}
