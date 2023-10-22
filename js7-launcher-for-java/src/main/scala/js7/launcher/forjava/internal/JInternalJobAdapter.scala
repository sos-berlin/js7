package js7.launcher.forjava.internal

import js7.base.problem.Checked
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext
import cats.effect.IO
import scala.jdk.FutureConverters.CompletionStageOps

private[js7] final class JInternalJobAdapter(jobContext: JobContext)
extends InternalJob:

  private val helper = new InternalJobAdapterHelper[JInternalJob]

  override def start: IO[Checked[Unit]] =
    helper.callStart(
      JInternalJob.JobContext(jobContext),
      job => IO.fromFuture(job.start.asScala))

  override def stop: IO[Unit] =
    helper.callStop(
      job => IO.fromFuture(job.stop.asScala).void)

  def toOrderProcess(step: Step) =
    import jobContext.implicitJs7Scheduler
    val jStep = JInternalJob.Step(step)
    helper.callProcessOrder(_
      .toOrderProcess(jStep).asScala)
