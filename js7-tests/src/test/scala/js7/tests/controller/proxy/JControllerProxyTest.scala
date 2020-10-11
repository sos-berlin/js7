package js7.tests.controller.proxy

import io.circe.syntax._
import java.nio.file.Files.createDirectory
import java.util.concurrent.TimeUnit.SECONDS
import java.util.concurrent.TimeoutException
import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Lazy
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentName
import js7.data.controller.ControllerItems.jsonCodec
import js7.data.item.{InventoryItem, VersionId}
import js7.data.job.ExecutablePath
import js7.data.workflow.WorkflowPath
import js7.proxy.javaapi.JProxyContext
import js7.proxy.javaapi.data.auth.{JAdmission, JCredentials, JHttpsConfig}
import js7.tests.controller.proxy.ClusterProxyTest.workflow
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.CancellationException
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try}

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

  protected val agentNames = AgentName("AGENT") :: Nil
  protected val inventoryItems = Nil

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

    directoryProvider.agents.head.writeExecutable(ExecutablePath("/TEST.cmd"), script(1.s))
  }

  "JControllerProxy" in {
    directoryProvider.runAgents() { _ =>
      val port = findFreeTcpPort()
      val controller = Lazy { directoryProvider.startController(httpPort = Some(port)).await(99.s) }
      try {
        val admissions = List(JAdmission.of(s"http://127.0.0.1:$port", ClusterProxyTest.primaryCredentials)).asJava
        val myVersionId = VersionId("MY-VERSION")
        JControllerProxyTester.run(admissions, JHttpsConfig.empty,
          List[InventoryItem](
            workflow.withVersion(myVersionId),
            workflow.withId(WorkflowPath("/B-WORKFLOW") ~ myVersionId),
          ).map(_.asJson.compactPrint).asJava,
          (1 to 1000).map(i => workflow.withId(WorkflowPath(s"/WORKFLOW-$i") ~ myVersionId))
            .map(_.asJson.compactPrint).asJava,
          () => controller())
      } finally
        for (controller <- controller) {
          controller.terminate() await 99.s
          controller.close()
        }
    }
  }

  "cancel startProxy" in {
    val admissions = List(JAdmission.of(s"http://127.0.0.1:0", JCredentials.noCredentials)).asJava
    autoClosing(new JProxyContext) { context =>
      val api = context.newControllerApi(admissions, JHttpsConfig.empty)
      val future = api.startProxy()
      Try(future.get(2, SECONDS)) match {
        case Failure(_: TimeoutException) =>
        case o => fail(s"startProxy must not complete: $o")
      }
      assert(!future.isDone)
      future.cancel(false)
      val tried = Try(future.get(9, SECONDS))
      assert(tried.failed.toOption.exists(_.isInstanceOf[CancellationException]))
    }
  }
}
