package js7.tests.controller.cluster

import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.controller.ControllerEvent.ControllerReady
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderFinished, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.JobResourceControllerClusterTest.*
import js7.tests.jobs.EmptyJob
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable

final class JobResourceControllerClusterTest extends ControllerClusterTester
{
  "Cluster handles JobResources properly" in {
    withControllerAndBackupWithoutAgents() { (primary, backup, _) =>
      primary.runAgents() { _ =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()
            primaryController.updateItemsAsSystemUser(Observable(
              AddOrChangeSigned(primary.itemSigner.toSignedString(jobResource)),
              AddVersion(versionId),
              AddOrChangeSigned(primary.itemSigner.toSignedString(workflow))
            )).await(99.s).orThrow

            assert(primaryController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            assert(backupController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            val stamped = primaryController.runOrder(FreshOrder(OrderId("A"), workflow.path))
            assert(stamped.map(_.value).collectFirst { case o: OrderTerminated => o } == Some(OrderFinished()))
          }
        }
      }

      primary.runAgents() { _ =>
        backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
          primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()
            primaryController.eventWatch.await[ControllerReady]()
            primaryController.waitUntilReady()

            assert(primaryController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            assert(backupController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            val stamped = primaryController.runOrder(FreshOrder(OrderId("B"), workflow.path))
            assert(stamped.map(_.value).collectFirst { case o: OrderTerminated => o } == Some(OrderFinished()))
          }
        }
      }
    }
  }
}

object JobResourceControllerClusterTest
{
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val jobResource0 = jobResource.copy(itemRevision = Some(ItemRevision(0)))
  private val versionId = VersionId("1")
  private val workflow = Workflow(WorkflowPath("JOB-RESOURCE-WORKFLOW") ~ versionId,
    Vector(
      EmptyJob.execute(AgentPath("AGENT"),
        jobResourcePaths = Seq(jobResource.path))))
}
