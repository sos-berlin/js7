package js7.launcher.forjava.internal

import cats.effect.IO
import js7.base.catsutils.CatsEffectExtensions.blockingOn
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext

private final class BlockingInternalJobAdapter(jobContext: JobContext)
extends InternalJob:

  import jobContext.blockingJobEC

  private val helper = new InternalJobAdapterHelper[BlockingInternalJob](blockingJobEC)

  override def start: IO[Checked[Unit]] =
    helper.callStart(
      BlockingInternalJob.JobContext(jobContext),
      job =>
        IO.blockingOn(blockingJobEC):
          job.start())

  override def stop: IO[Unit] =
    helper.callStop: job =>
      IO.blockingOn(blockingJobEC):
        job.stop()

  def toOrderProcess(step: Step) =
    val jStep = BlockingInternalJob.Step(step)(using jobContext.ioRuntime)

    helper.callProcessOrder: jInternalJob =>
      val memoizedOrderProcess: IO[BlockingInternalJob.OrderProcess] =
        memoize:
          IO.blockingOn(blockingJobEC):
            jInternalJob.toOrderProcess(jStep)

      new OrderProcess:
        protected def run =
          for
            orderProcess <- memoizedOrderProcess
            fiber <-
              IO.blockingOn(blockingJobEC):
                orderProcess.run()
              .map(_.asScala)
              .start
          yield
            fiber

        def cancel(immediately: Boolean) =
          orderProcessIO.flatMap: orderProcess =>
            IO.blockingOn(blockingJobEC):
              orderProcess.cancel(immediately)

        override def toString =
          s"BlockingINternalJob(${jobContext.implementationClass.scalaName}) OrderProcess"
