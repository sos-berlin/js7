package js7.launcher.forjava.internal

import cats.effect.IO
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
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
      .evalOn(jobContext.blockingJobScheduler)

  override def stop: IO[Unit] =
    helper.callStop(job => IO(job.stop()))
      .evalOn(jobContext.blockingJobScheduler)

  def toOrderProcess(step: Step) =
    val jStep = BlockingInternalJob.Step(step)(using jobContext.ioRuntime)

    helper.callProcessOrder { jInternalJob =>
      val orderProcessIO: IO[BlockingInternalJob.OrderProcess] =
        IO { jInternalJob.toOrderProcess(jStep) }
          .evalOn(jobContext.blockingJobScheduler)
          .unsafeMemoize

      new OrderProcess:
        protected def run =
          for
            orderProcess <- orderProcessIO
            fiber <- IO(orderProcess.run())
              .evalOn(jobContext.blockingJobScheduler)
              .map(_.asScala)
              .start
          yield
            fiber

        def cancel(immediately: Boolean) =
          orderProcessIO.flatMap(orderProcess =>
            IO(orderProcess.cancel(immediately))
              .evalOn(jobContext.blockingJobScheduler))

        override def toString =
          s"BlockingINternalJob(${jobContext.implementationClass.scalaName}) OrderProcess"
    }
