package js7.launcher.forjava.internal

import io.vavr.control.{Either => VEither}
import java.io.IOException
import java.util.concurrent.{CompletableFuture, CompletionStage}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data_for_java.common.JavaUtils.Void
import js7.launcher.forjava.internal.JInternalJob._
import js7.launcher.internal.{InternalJob, InternalJobAdapter}
import monix.execution.Ack.{Continue, Stop}
import monix.execution.Scheduler
import monix.reactive.Observer
import scala.compat.java8.FutureConverters._
import scala.concurrent.Future

@InternalJobAdapter(classOf[JInternalJobAdapter])
trait JInternalJob
{
  @Nonnull
  def start: CompletionStage[VEither[Problem, Void]] =
    CompletableFuture.completedFuture(VEither.right(Void))
    // Since Java 9: CompletableFuture.completedStage(Void)

  @Nonnull
  final def stop: CompletionStage[Void] =
    CompletableFuture.completedFuture(Void)

  @Nonnull
  def toOrderProcess(@Nonnull step: Step): JOrderProcess
}

object JInternalJob
{
  final case class JobContext(asScala: InternalJob.JobContext)
  extends JavaJobContext

  final case class Step(asScala: InternalJob.Step)(private implicit val s: Scheduler)
  extends JavaJobStep
  {
    def sendOut(string: String): CompletionStage[Void] =
      send(string, asScala.outObserver)

    def sendErr(string: String): CompletionStage[Void] =
      send(string, asScala.errObserver)

    private def send(string: String, observer: Observer[String]): CompletionStage[Void] =
      observer.onNext(string).flatMap {
        case Stop => Future.failed(new IOException("Stream closed"))
        case Continue => Future.successful(Void)
      }.toJava
  }
}
