package js7.tests.cluster.controller

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.OptionalLong
import java.util.function.Function.identity
import js7.base.auth.Admission
import js7.base.log.LogLevel
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.node.Js7ServerId
import js7.data.subagent.SubagentId
import js7.proxy.javaapi.JControllerApi
import js7.tests.testenv.ControllerClusterForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.jdk.CollectionConverters.*

final class LogFileClusterTest extends OurTestSuite, ControllerClusterForScalaTest:

  protected def items = Nil

  private given IORuntime = ioRuntime

  "Primary Controller log" in:
    withControllerAndBackup(): (primary, _, backup, _, _) =>
      primary.runController(): controller =>
        val admission = primary.controllerAdmission(controller.runningController)
        getLog(admission, Js7ServerId.primaryController).map: line =>
          assert(line.contains("TEST ONLY: Controller/primary, "))
        .await(99.s)

  "Backup Controller log" in :
    runControllerAndBackup(): (primary, primaryController, _, backup, backupController, _, _) =>
      val admission = backup.controllerAdmission(backupController.runningController)
      getLog(admission, Js7ServerId.primaryController).map: line =>
        assert(line.contains("TEST ONLY: Controller/backup, "))
      .await(99.s)

  private def getLog(admission: Admission, serverId: Js7ServerId): IO[String] =
    JControllerApi.run(admissions = admission :: Nil): jControllerApi =>
      jControllerApi.runControllerProxy: jControllerProxy =>
        jControllerProxy
          .rawLogLineFlux(
            serverId, LogLevel.None /*test*/ , begin = Instant.now, lines = OptionalLong.of(1))
          .flatMapIterable(identity)
          .map(new String(_, UTF_8))
          .collectList()
          .map(_.asScala)
          .map: lines =>
            assert(lines.size == 1)
            lines.head
          .toFuture


object LogFileClusterTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val bareSubagentId = SubagentId("BARE-SUBAGENT")
