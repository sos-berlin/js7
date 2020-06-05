package js7.common.http

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.ExitCase
import com.typesafe.scalalogging.Logger
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
object StreamingSupport
{
  private val logger = Logger("js7.common.http.StreamingSupport")  // TODO Use Logger adapter (unreachable in module common)

  implicit final class AkkaObservable[A](private val underlying: Observable[A]) extends AnyVal {
    def toAkkaSource(implicit scheduler: Scheduler, A: TypeTag[A]): Source[A, NotUsed] =
      Source.fromPublisher(
        underlying
          .guaranteeCase {
            case ExitCase.Completed => Task.unit
            case exitCase => Task { logger.trace(s"Observable[${A.tpe}] toAkkaSource: $exitCase") }
          }
          .toReactivePublisher(scheduler))
  }

  implicit final class ObservableAkkaSource[Out, Mat](private val underlying: Source[Out, Mat]) extends AnyVal {
    def toObservable(implicit materializer: Materializer): Observable[Out] =
      Observable.fromReactivePublisher(
        underlying.runWith(Sink.asPublisher(fanout = false)))
  }
}
