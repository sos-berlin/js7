package js7.tests.jobs

import cats.syntax.traverse._
import java.nio.file.Files.deleteIfExists
import java.nio.file.{Path, Paths}
import js7.base.io.process.Stderr
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax._
import js7.data.order.Outcome
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.JobContext
import js7.tests.jobs.DeleteFileJob.logger
import monix.eval.Task

final class DeleteFileJob(jobContext: JobContext) extends InternalJob
{
  def toOrderProcess(step: Step) =
    OrderProcess(
      step.arguments.checked("file")
        .orElse(step.order.arguments.checked(FileArgumentName))
        .flatMap(_.toStringValueString)
        .map(Paths.get(_))
        .traverse(deleteFile(_, step.send(Stderr, _).void))
        .rightAs(Outcome.succeeded)
        .map(Outcome.Completed.fromChecked))

  private def deleteFile(file: Path, out: String => Task[Unit]): Task[Unit] =
    Task(deleteIfExists(file))
      .executeOn(jobContext.ioExecutor.scheduler)
      .flatMap {
        case false => Task.unit
        case true =>
          logger.info(s"Deleted $file")
          out(s"Deleted $file\n")
      }
}

object DeleteFileJob extends InternalJob.Companion[DeleteFileJob]
{
  private val logger = Logger[this.type]
}
