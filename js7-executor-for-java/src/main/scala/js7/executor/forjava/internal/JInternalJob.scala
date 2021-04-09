package js7.executor.forjava.internal

import io.vavr.control.{Either => VEither}
import java.io.IOException
import java.util.concurrent.{CompletableFuture, CompletionStage}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data_for_java.common.JavaUtils.Void
import js7.executor.forjava.internal.JInternalJob._
import js7.executor.internal.InternalJob.{JobContext, OrderContext}
import js7.executor.internal.InternalJobAdapter
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
  def processOrder(@Nonnull orderContext: JOrderContext): JOrderProcess
}

object JInternalJob
{
  final case class JJobContext(asScala: JobContext)
  extends JavaJobContext

  final case class JOrderContext(asScala: OrderContext)(private implicit val s: Scheduler)
  extends JavaOrderContext
  {
    def sendOut(string: String): CompletionStage[Void] =
      send(string, asScala.outObserver)

    def sendErr(string: String): CompletionStage[Void] = {
      send(string, asScala.errObserver)
    }

    private def send(string: String, observer: Observer[String]): CompletionStage[Void] =
      observer.onNext(string).flatMap {
        case Stop => Future.failed(new IOException("Stream closed"))
        case Continue => Future.successful(Void)
      }.toJava
  }
}
