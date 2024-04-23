package js7.tests.jobresource

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.item.ItemOperation
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.tests.jobresource.JobResourceTest.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import fs2.Stream

final class JobResourceAtBareSubagentTest extends JobResourceTest:

  override protected val agentConfig = config"""
    js7.auth.subagents.BARE-SUBAGENT = "${toLocalSubagentId(agentPath).string}'s PASSWORD"
    """.withFallback(super.agentConfig)

  private lazy val bareSubagentItem = SubagentItem(
    SubagentId("BARE-SUBAGENT"),
    agentPath,
    findFreeLocalUri())

  private lazy val (bSubagent, bSubagentRelease) =
    directoryProvider.bareSubagentResource(bareSubagentItem).allocated.await(99.s)

  override def beforeAll() =
    super.beforeAll()
    controller.api
      .updateItems(Stream(
        ItemOperation.AddOrChangeSimple(bareSubagentItem),
        ItemOperation.AddOrChangeSimple(directoryProvider.subagentItems(0).copy(disabled = true))))
      .await(99.s)
      .orThrow
    bSubagent // Start Subagent

  override def afterAll() =
    try
      bSubagentRelease.await(99.s)
    finally
      super.afterAll()
