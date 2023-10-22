package js7.launcher.forjava.internal

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext

private final class BlockingInternalJobAdapter(jobContext: JobContext)
extends InternalJob:
  private val helper = new InternalJobAdapterHelper[BlockingInternalJob]

  override def start: IO[Checked[Unit]] =
    helper.callStart(BlockingInternalJob.JobContext(jobContext), job => IO(job.start()))
      .executeOn(jobContext.blockingJobScheduler)

  override def stop: IO[Unit] =
    helper.callStop(job => IO(job.stop()))
      .executeOn(jobContext.blockingJobScheduler)

  def toOrderProcess(step: Step) =
    val jStep = BlockingInternalJob.Step(step)(jobContext.js7Scheduler)

    helper.callProcessOrder { jInternalJob =>
      val orderProcessIO: IO[BlockingInternalJob.OrderProcess] =
        IO { jInternalJob.toOrderProcess(jStep) }
          .executeOn(jobContext.blockingJobScheduler)
          .memoize

      new OrderProcess:
        protected def run =
          for
            orderProcess <- orderProcessIO
            fiber <- IO(orderProcess.run())
              .executeOn(jobContext.blockingJobScheduler)
              .map(_.asScala)
              .start
          yield
            fiber

        def cancel(immediately: Boolean) =
          orderProcessIO.flatMap(orderProcess =>
            IO(orderProcess.cancel(immediately))
              .executeOn(jobContext.blockingJobScheduler))

        override def toString =
          s"BlockingINternalJob(${jobContext.implementationClass.scalaName}) OrderProcess"
    }
