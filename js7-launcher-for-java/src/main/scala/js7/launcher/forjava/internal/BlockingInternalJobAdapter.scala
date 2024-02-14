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
    helper
      .callStart(
        BlockingInternalJob.JobContext(jobContext),
        job => IO
          .blocking:
            job.start()
          .evalOn(jobContext.blockingJobEC))

  override def stop: IO[Unit] =
    helper
      .callStop(job => IO
        .blocking:
          job.stop()
        .evalOn(jobContext.blockingJobEC))

  def toOrderProcess(step: Step) =
    val jStep = BlockingInternalJob.Step(step)(using jobContext.ioRuntime)

    helper.callProcessOrder { jInternalJob =>
      val orderProcessIO: IO[BlockingInternalJob.OrderProcess] =
        IO
          .blocking:
            jInternalJob.toOrderProcess(jStep)
          .evalOn(jobContext.blockingJobEC)
          .unsafeMemoize

      new OrderProcess:
        protected def run =
          for
            orderProcess <- orderProcessIO
            fiber <- IO
              .blocking:
                orderProcess.run()
              .evalOn(jobContext.blockingJobEC)
              .map(_.asScala)
              .start
          yield
            fiber

        def cancel(immediately: Boolean) =
          orderProcessIO.flatMap(orderProcess =>
            IO
              .blocking:
                orderProcess.cancel(immediately)
              .evalOn(jobContext.blockingJobEC))

        override def toString =
          s"BlockingINternalJob(${jobContext.implementationClass.scalaName}) OrderProcess"
    }
