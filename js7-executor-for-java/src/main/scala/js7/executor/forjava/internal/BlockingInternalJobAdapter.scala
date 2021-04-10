package js7.executor.forjava.internal

import js7.base.problem.Checked
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.JobContext
import monix.eval.Task

private final class BlockingInternalJobAdapter(jobContext: JobContext)
extends InternalJob
{
  private val helper = new InternalJobAdapterHelper[BlockingInternalJob]

  override def start: Task[Checked[Unit]] =
    helper.callStart(BlockingInternalJob.JobContext(jobContext), job => Task(job.start()))
      .executeOn(jobContext.blockingJobScheduler)

  override def stop: Task[Unit] =
    helper.callStop(job => Task(job.
      stop()))
      .executeOn(jobContext.blockingJobScheduler)

  def processOrder(step: Step) = {
    import jobContext.js7Scheduler
    val jStep = BlockingInternalJob.Step(step)
    helper.callProcessOrder(jInternalJob =>
      OrderProcess(
        Task { jInternalJob.processOrder(jStep) }
          .executeOn(jobContext.blockingJobScheduler)
          .guarantee(Task {
            jStep.close()
          })
          .map(_.asScala)))
  }
}
