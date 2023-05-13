package js7.subagent.director

import cats.syntax.all.*
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.SignableItem
import js7.data.job.{JobConf, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentDirectorState, SubagentId, SubagentItem}
import js7.data.workflow.Workflow
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.Journal
import monix.eval.{Fiber, Task}
import scala.annotation.unused

trait SubagentDriver {

  def subagentItem: SubagentItem

  final def subagentId: SubagentId =
    subagentItem.id

  def isCoupled: Boolean

  def isLocal: Boolean

  protected val journal: Journal[? <: SubagentDirectorState[?]]

  def startOrderProcessing(order: Order[Order.Processing]): Task[Checked[Fiber[OrderProcessed]]]

  def recoverOrderProcessing(order: Order[Order.Processing]): Task[Checked[Fiber[OrderProcessed]]]

  // TODO Emit one batch for all recovered orders!
  final def emitOrderProcessLost(order: Order[Order.Processing])
  : Task[Checked[OrderProcessed]] =
    journal
      .persistKeyedEvent(order.id <-: OrderProcessed.processLostDueToRestart)
      .map(_.map(_._1.value.event))

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit]

  def tryShutdown: Task[Unit]

  def terminate(@unused signal: Option[ProcessSignal]): Task[Unit]

  protected final def signableItemsForOrderProcessing(workflowPosition: WorkflowPosition)
  : Task[Checked[Seq[Signed[SignableItem]]]] =
    for (s <- journal.state) yield
      for {
        signedWorkflow <- s.keyToSigned(Workflow).checked(workflowPosition.workflowId)
        workflow = signedWorkflow.value
        jobKey <- workflow.positionToJobKey(workflowPosition.position)
        job <- workflow.keyToJob.checked(jobKey)
        jobResourcePaths = JobConf.jobResourcePathsFor(job, workflow)
        signedJobResources <- jobResourcePaths.traverse(s.keyToSigned(JobResource).checked)
      } yield signedJobResources :+ signedWorkflow
}
