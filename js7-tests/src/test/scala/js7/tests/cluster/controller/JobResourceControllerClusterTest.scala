package js7.tests.cluster.controller

import js7.base.problem.Checked.Ops
import js7.base.thread.CatsBlocking.syntax.*
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
import js7.tests.cluster.controller.JobResourceControllerClusterTest.*
import js7.tests.jobs.EmptyJob
import fs2.Stream
import js7.data.event.EventId

final class JobResourceControllerClusterTest extends ControllerClusterTester:

  "Cluster handles JobResources properly" in:
    withControllerAndBackupWithoutAgents(): (primary, backup, _) =>
      var eventId = EventId.BeforeFirst
      primary.runAgents(): _ =>
        backup.runController(dontWaitUntilReady = true): backupController =>
          primary.runController(): primaryController =>
            primaryController.eventWatch.await[ClusterCoupled]()
            primaryController.updateItemsAsSystemUser:
              Stream(
                AddOrChangeSigned(primary.itemSigner.toSignedString(jobResource)),
                AddVersion(versionId),
                AddOrChangeSigned(primary.itemSigner.toSignedString(workflow)))
            .await(99.s).orThrow

            assert(primaryController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            assert(backupController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            val stamped = primaryController.runOrder(FreshOrder(OrderId("A"), workflow.path))
            assert(stamped.map(_.value).collectFirst { case o: OrderTerminated => o } == Some(OrderFinished()))
            eventId = primaryController.eventWatch.lastAddedEventId

      primary.runAgents(): _ =>
        backup.runController(dontWaitUntilReady = true): backupController =>
          primary.runController(): primaryController =>
            primaryController.eventWatch.await[ClusterCoupled](after = eventId)
            primaryController.eventWatch.await[ControllerReady](after = eventId)
            primaryController.waitUntilReady()

            assert(primaryController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            assert(backupController.controllerState().pathToSignedSimpleItem(jobResource.path).value == jobResource0)
            val stamped = primaryController.runOrder(FreshOrder(OrderId("B"), workflow.path))
            assert(stamped.map(_.value).collectFirst { case o: OrderTerminated => o } == Some(OrderFinished()))


object JobResourceControllerClusterTest:
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val jobResource0 = jobResource.copy(itemRevision = Some(ItemRevision(0)))
  private val versionId = VersionId("1")
  private val workflow = Workflow(WorkflowPath("JOB-RESOURCE-WORKFLOW") ~ versionId,
    Vector(
      EmptyJob.execute(AgentPath("AGENT"),
        jobResourcePaths = Seq(jobResource.path))))
