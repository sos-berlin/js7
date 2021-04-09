package js7.executor.forjava.internal

import js7.base.problem.Checked
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{JobContext, OrderContext}
import monix.eval.Task

private final class BlockingInternalJobAdapter(jobContext: JobContext)
extends InternalJob
{
  private val helper = new InternalJobAdapterHelper[BlockingInternalJob]

  override def start: Task[Checked[Unit]] =
    helper.callStart(BlockingInternalJob.JJobContext(jobContext), job => Task(job.start()))
      .executeOn(jobContext.blockingJobScheduler)

  override def stop: Task[Unit] =
    helper.callStop(job => Task(job.
      stop()))
      .executeOn(jobContext.blockingJobScheduler)

  def processOrder(orderContext: OrderContext) = {
    import jobContext.js7Scheduler
    val jOrderContext = BlockingInternalJob.JOrderContext(orderContext,
      outWriter = new ObserverWriter(orderContext.outObserver),
      errWriter = new ObserverWriter(orderContext.errObserver))
    val orderProcess = helper.processOrder(jOrderContext)(
      (jInternalJob, jOrderContext) =>
        OrderProcess(
          Task { jInternalJob.processOrder(jOrderContext) }
            .executeOn(jobContext.blockingJobScheduler)
            .map(_.asScala)))
    orderProcess.copy(
      run = orderProcess.run.guarantee(Task(
        jOrderContext.close())))
  }
}
