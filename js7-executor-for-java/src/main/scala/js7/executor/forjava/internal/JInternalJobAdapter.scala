package js7.executor.forjava.internal

import js7.base.problem.Checked
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{JobContext, OrderContext}
import monix.eval.Task
import scala.jdk.FutureConverters.CompletionStageOps

private[js7] final class JInternalJobAdapter(jobContext: JobContext)
extends InternalJob
{
  private val helper = new InternalJobAdapterHelper[JInternalJob]

  override def start: Task[Checked[Unit]] =
    helper.start(
      JInternalJob.JJobContext(jobContext),
      job => Task.fromFuture(job.start.asScala).void)

  def processOrder(context: OrderContext) = {
    import jobContext.js7Scheduler
    val jOrderContext = JInternalJob.JOrderContext(context)
    helper.processOrder(jOrderContext)((jInternalJob, jOrderContext) =>
      jInternalJob.processOrder(jOrderContext).asScala)
  }
}
