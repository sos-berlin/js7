package js7.executor.forjava.internal

import js7.base.problem.Checked
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.JobContext
import monix.eval.Task
import scala.jdk.FutureConverters.CompletionStageOps

private[js7] final class JInternalJobAdapter(jobContext: JobContext)
extends InternalJob
{
  private val helper = new InternalJobAdapterHelper[JInternalJob]

  override def start: Task[Checked[Unit]] =
    helper.callStart(
      JInternalJob.JobContext(jobContext),
      job => Task.fromFuture(job.start.asScala))

  override def stop: Task[Unit] =
    helper.callStop(
      job => Task.fromFuture(job.stop.asScala).as(()))

  def processOrder(step: Step) = {
    import jobContext.js7Scheduler
    val jStep = JInternalJob.Step(step)
    helper.processOrder(jStep)((jInternalJob, jStep) =>
      jInternalJob.processOrder(jStep).asScala)
  }
}
