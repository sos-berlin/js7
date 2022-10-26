package js7.tests.controller.cluster

import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerEvent.ControllerReady
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderFinished, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.JobResourceClusterTest.*
import js7.tests.jobs.EmptyJob
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

final class JobResourceClusterTest extends ControllerClusterTester
{
  "Cluster handles JobResources properly" in {
    withControllerAndBackupWithoutAgents() { (primary, backup, _) =>
      primary.runAgents() { _ =>
        val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
        val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s

        primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()

        primaryController.updateItemsAsSystemUser(Observable(
          AddOrChangeSigned(primary.itemSigner.toSignedString(jobResource)),
          AddVersion(versionId),
          AddOrChangeSigned(primary.itemSigner.toSignedString(workflow))
        )).await(99.s).orThrow

        assert(primaryController.controllerState.await(99.s).pathToSignedSimpleItem(jobResource.path).value == jobResource0)
        assert(backupController.controllerState.await(99.s).pathToSignedSimpleItem(jobResource.path).value == jobResource0)
        val stamped = primaryController.runOrder(FreshOrder(OrderId("A"), workflow.path))
        assert(stamped.map(_.value).collectFirst { case o: OrderTerminated => o } == Some(OrderFinished()))

        primaryController.terminate() await 99.s
        backupController.terminate() await 99.s
      }

      primary.runAgents() { _ =>
        val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
        val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
        primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()
        primaryController.eventWatch.await[ControllerReady]()
        sleep(100.ms)  // TODO Wait controllerState has been updated

        assert(primaryController.controllerState.await(99.s).pathToSignedSimpleItem(jobResource.path).value == jobResource0)
        assert(backupController.controllerState.await(99.s).pathToSignedSimpleItem(jobResource.path).value == jobResource0)
        val stamped = primaryController.runOrder(FreshOrder(OrderId("B"), workflow.path))
        assert(stamped.map(_.value).collectFirst { case o: OrderTerminated => o } == Some(OrderFinished()))

        primaryController.terminate() await 99.s
        backupController.terminate() await 99.s
      }
    }
  }
}

object JobResourceClusterTest
{
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val jobResource0 = jobResource.copy(itemRevision = Some(ItemRevision(0)))
  private val versionId = VersionId("1")
  private val workflow = Workflow(WorkflowPath("JOB-RESOURCE-WORKFLOW") ~ versionId,
    Vector(
      EmptyJob.execute(AgentPath("AGENT"),
        jobResourcePaths = Seq(jobResource.path))))
}
