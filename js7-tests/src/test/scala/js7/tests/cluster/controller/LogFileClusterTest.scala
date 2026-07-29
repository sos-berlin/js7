package js7.tests.cluster.controller

import cats.effect.unsafe.IORuntime
import fs2.Stream
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.concurrent.CompletableFuture
import java.util.function.Function.identity
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.LogLevel
import js7.base.log.reader.LogSelection
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsBlocking.BlockingIOResource
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentPath
import js7.data.item.ItemOperation
import js7.data.node.Js7ServerId
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.proxy.javaapi.log.JLogSelection
import js7.proxy.javaapi.{JControllerApi, JControllerProxy}
import js7.tests.cluster.controller.LogFileClusterTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.jdk.CollectionConverters.*


final class LogFileClusterTest extends OurTestSuite, ControllerClusterForScalaTest:

  protected def items = Nil
  private lazy val bareSubagentItem = SubagentItem(bareSubagentId, agentPath, findFreeLocalUri())

  private given IORuntime = ioRuntime


  "byteLogLineFlux, Primary and Backup Controller, Director and bare Subagent" in:
    val agentConfig = config"""
      js7.auth.subagents.BARE-SUBAGENT = "${toLocalSubagentId(agentPath).string}'s PASSWORD"
      """

    runControllerAndBackup(
      agentConfig = agentConfig
    ): (primary, primaryController, _, backup, backupController, _, _) =>
      primary.bareSubagentResource(bareSubagentItem).blockingUse(99.s): bareSubagent =>
        primaryController.api.updateItems:
          Stream(ItemOperation.AddOrChangeSimple(bareSubagentItem))
        .await(99.s).orThrow

        val admissions = List(
          primary.controllerAdmission(primaryController.runningController),
          backup.controllerAdmission(backupController.runningController))
        JControllerApi.run(admissions): jControllerApi =>
          jControllerApi.runControllerProxy: jControllerProxy =>
            getLog(jControllerProxy, Js7ServerId.primaryController).thenAccept: line =>
              assert(line.contains("TEST ONLY: Controller/primary"))
            .thenCompose: _ =>
              getLog(jControllerProxy, Js7ServerId.backupController).thenAccept: line =>
                assert(line.contains("TEST ONLY: Controller/secondary"))
            .thenCompose: _ =>
              getLog(jControllerProxy, Js7ServerId.Subagent(subagentId)).thenAccept: line =>
                assert(line.contains(s"TEST ONLY: $subagentId, "))
            .thenCompose: _ =>
              getLog(jControllerProxy, Js7ServerId.Subagent(bareSubagentId)).thenAccept: line =>
                assert(line.contains(s"TEST ONLY: $bareSubagentId, "))
        .await(99.s)


  private def getLog(jControllerProxy: JControllerProxy, serverId: Js7ServerId)
  : CompletableFuture[String] =
    // Get twice to let you count logins. Should be only one login.
    getLogSingle(jControllerProxy, serverId).thenCompose: result1 =>
      getLogSingle(jControllerProxy, serverId).thenApply: result2 =>
        assert(result1 == result2)
        result1

  private def getLogSingle(jControllerProxy: JControllerProxy, serverId: Js7ServerId)
  : CompletableFuture[String] =
    jControllerProxy
      .byteLogLineFlux(
        serverId, LogLevel.None /*test*/ , begin = Instant.now,
        JLogSelection(LogSelection(lineLimit = Some(1))))
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
