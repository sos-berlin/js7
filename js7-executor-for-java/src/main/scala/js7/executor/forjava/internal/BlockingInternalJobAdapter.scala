package js7.executor.forjava.internal

import js7.base.problem.Checked
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{JobContext, OrderContext, OrderProcess}
import monix.eval.Task

private final class BlockingInternalJobAdapter(jobContext: JobContext)
extends InternalJob
{
  private val helper = new InternalJobAdapterHelper[BlockingInternalJob]

  override def start: Task[Checked[Unit]] =
    helper.start(BlockingInternalJob.JJobContext(jobContext), job => Task(job.start()))
      .executeOn(jobContext.blockingJobScheduler)

  def processOrder(context: OrderContext) = {
    import jobContext.js7Scheduler
    val jOrderContext = BlockingInternalJob.JOrderContext(context,
      outWriter = new ObserverWriter(context.out),
      errWriter = new ObserverWriter(context.err))
    val orderProcess = helper.processOrder(jOrderContext)(
      (jInternalJob, jOrderContext) =>
        OrderProcess(
          Task { jInternalJob.processOrder(jOrderContext) }
            .executeOn(jobContext.blockingJobScheduler)
            .map(_.asScala)
            .materialize
            .map(Checked.fromTry)))
    orderProcess.copy(
      completed = orderProcess.completed.guarantee(Task(
        jOrderContext.close())))
  }
}
