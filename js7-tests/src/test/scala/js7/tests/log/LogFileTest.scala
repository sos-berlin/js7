package js7.tests.log

import cats.effect.IO
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.OptionalLong
import java.util.function.Function.identity
import js7.base.configutils.Configs.*
import js7.base.log.LogLevel
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentPath
import js7.data.node.Js7ServerId
import js7.data.subagent.SubagentItemStateEvent.SubagentDedicated
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.proxy.javaapi.JControllerApi
import js7.tests.log.LogFileTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.jdk.CollectionConverters.*

/** See also LogFileClusterTest.
  */
final class LogFileTest extends OurAsyncTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected def agentPaths = Seq(agentPath)

  override protected def bareSubagentItems =
    SubagentItem(bareSubagentId, agentPath, findFreeLocalUri()) :: Nil

  protected def items = Nil

  "Primary Controller log" in:
    getLog(Js7ServerId.primaryController).map: line =>
      assert(line.contains("TEST ONLY: PrimaryController, "))

  "Backup Controller log" in:
    getLog(Js7ServerId.backupController).map: line =>
      assert(line.contains("TEST ONLY: PrimaryController, "))  // Because there is no backup

  "Director log" in:
    getLog(Js7ServerId.Subagent(subagentId)).map: line =>
      assert(line.contains(s"TEST ONLY: $subagentId, "))

  "Bare Subagent log" in:
    controller.awaitNextKey[SubagentDedicated](bareSubagentId)
    sleep(1.s)
    getLog(Js7ServerId.Subagent(bareSubagentId)).map: line =>
      assert(line.contains(s"TEST ONLY: $bareSubagentId, "))

  private def getLog(js7ServerId: Js7ServerId): IO[String] =
    JControllerApi.run(admissions = controllerAdmission :: Nil): jControllerApi =>
      jControllerApi.runControllerProxy: jControllerProxy =>
        jControllerProxy
          .rawLogLineFlux(
            js7ServerId, LogLevel.None /*test*/ , begin = Instant.now, lines = OptionalLong.of(1))
          .flatMapIterable(identity)
          .map(new String(_, UTF_8))
          .collectList()
          .map(_.asScala)
          .map: lines =>
            assert(lines.size == 1)
            lines.head
          .toFuture


object LogFileTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val bareSubagentId = SubagentId("BARE-SUBAGENT")
