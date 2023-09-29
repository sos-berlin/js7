package js7.launcher.forjava.internal

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext
import monix.eval.Task

private final class BlockingInternalJobAdapter(jobContext: JobContext)
extends InternalJob:
  private val helper = new InternalJobAdapterHelper[BlockingInternalJob]

  override def start: Task[Checked[Unit]] =
    helper.callStart(BlockingInternalJob.JobContext(jobContext), job => Task(job.start()))
      .executeOn(jobContext.blockingJobScheduler)

  override def stop: Task[Unit] =
    helper.callStop(job => Task(job.stop()))
      .executeOn(jobContext.blockingJobScheduler)

  def toOrderProcess(step: Step) =
    val jStep = BlockingInternalJob.Step(step)(jobContext.js7Scheduler)

    helper.callProcessOrder { jInternalJob =>
      val orderProcessTask: Task[BlockingInternalJob.OrderProcess] =
        Task { jInternalJob.toOrderProcess(jStep) }
          .executeOn(jobContext.blockingJobScheduler)
          .memoize

      new OrderProcess:
        protected def run =
          for
            orderProcess <- orderProcessTask
            fiber <- Task(orderProcess.run())
              .executeOn(jobContext.blockingJobScheduler)
              .map(_.asScala)
              .start
          yield
            fiber

        def cancel(immediately: Boolean) =
          orderProcessTask.flatMap(orderProcess =>
            Task(orderProcess.cancel(immediately))
              .executeOn(jobContext.blockingJobScheduler))

        override def toString =
          s"BlockingINternalJob(${jobContext.implementationClass.scalaName}) OrderProcess"
    }
