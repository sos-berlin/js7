package js7.tests.jobresource

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.item.ItemOperation
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.tests.jobresource.JobResourceTest.agentPath
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

final class JobResourceAtBareSubagentTest extends JobResourceTest
{
  override protected val agentConfig = config"""
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
    """.withFallback(super.agentConfig)

  private lazy val bSubagentItem = SubagentItem(
    SubagentId("B-SUBAGENT"),
    agentPath,
    Uri("http://localhost:" + findFreeTcpPort()))

  private lazy val (bSubagent, bSubagentRelease) =
    directoryProvider.subagentResource(bSubagentItem).allocated.await(99.s)

  override def beforeAll() = {
    super.beforeAll()
    controllerApi
      .updateItems(Observable(
        ItemOperation.AddOrChangeSimple(bSubagentItem),
        ItemOperation.AddOrChangeSimple(directoryProvider.subagentItems(0).copy(disabled = true))))
      .await(99.s)
      .orThrow
    bSubagent // Start Subagent
  }

  override def afterAll() = {
    bSubagentRelease.await(99.s)
    super.afterAll()
  }
}
