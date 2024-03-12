package js7.launcher.forjava.internal

import cats.effect.unsafe.IORuntime
import io.vavr.control.Either as VEither
import java.util.concurrent.{CompletableFuture, CompletionStage}
import javax.annotation.Nonnull
import js7.base.io.process.{Stderr, Stdout}
import js7.base.problem.Problem
import js7.data_for_java.common.JavaUtils.Void
import js7.launcher.StdWriter
import js7.launcher.forjava.internal.JInternalJob.*
import js7.launcher.internal.{InternalJob, InternalJobAdapter}

@InternalJobAdapter(classOf[JInternalJobAdapter])
trait JInternalJob:

  @Nonnull
  def start: CompletionStage[VEither[Problem, Void]] =
    CompletableFuture.completedFuture(VEither.right(Void))

  @Nonnull
  final def stop: CompletionStage[Void] =
    CompletableFuture.completedFuture(Void)

  @Nonnull
  def toOrderProcess(@Nonnull step: Step): JOrderProcess


object JInternalJob:
  final case class JobContext(asScala: InternalJob.JobContext)
  extends JavaJobContext

  final case class Step(asScala: InternalJob.Step)(using IORuntime)
  extends JavaJobStep:

    def writeOut(string: String): CompletionStage[Void] =
      write(string, asScala.writer(Stdout))

    def writeErr(string: String): CompletionStage[Void] =
      write(string, asScala.writer(Stderr))

    private def write(string: String, stdWriter: StdWriter): CompletionStage[Void] =
      stdWriter.write(string).as(Void).unsafeToCompletableFuture()
