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

  def toOrderProcess(step: Step) = {
    import jobContext.js7Scheduler
    val jStep = BlockingInternalJob.Step(step)

    helper.callProcessOrder { jInternalJob =>
      val orderProcessTask: Task[BlockingInternalJob.OrderProcess] =
        Task { jInternalJob.toOrderProcess(jStep) }
          .executeOn(jobContext.blockingJobScheduler)
          .memoize

      new OrderProcess {
        protected def run =
          for {
            orderProcess <- orderProcessTask
            outcome <- Task(orderProcess.run()) executeOn jobContext.blockingJobScheduler
          } yield outcome.asScala

        override def cancel(immediately: Boolean) =
          orderProcessTask.flatMap(orderProcess =>
            Task(orderProcess.cancel(immediately))
              .executeOn(jobContext.blockingJobScheduler))
      }
    }
  }
}
