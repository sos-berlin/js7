package js7.tests.jobs

import cats.effect.IO
import cats.syntax.traverse.*
import java.nio.file.Files.deleteIfExists
import java.nio.file.{Path, Paths}
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderOutcome
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext
import js7.tests.jobs.DeleteFileJob.logger

final class DeleteFileJob(jobContext: JobContext) extends InternalJob:

  def toOrderProcess(step: Step) =
    OrderProcess(
      step.arguments.checked("file")
        .orElse(step.order.arguments.checked(FileArgumentName))
        .flatMap(_.toStringValueString)
        .map(Paths.get(_))
        .traverse(deleteFile(_, step.writeErr(_).void))
        .rightAs(OrderOutcome.succeeded)
        .map(OrderOutcome.Completed.fromChecked))

  private def deleteFile(file: Path, out: String => IO[Unit]): IO[Unit] =
    IO.interruptible(deleteIfExists(file))
      .flatMap:
        case false => IO.unit
        case true =>
          logger.info(s"Deleted $file")
          out(s"Deleted $file\n")


object DeleteFileJob extends InternalJob.Companion[DeleteFileJob]:
  private val logger = Logger[this.type]
