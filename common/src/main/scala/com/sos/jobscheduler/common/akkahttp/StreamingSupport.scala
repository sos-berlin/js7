package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.{ContentType, MediaType}
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import com.google.common.base.Ascii
import com.sos.jobscheduler.base.utils.CloseableIterator
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
object StreamingSupport
{
  val `application/json-seq` = MediaType.customWithFixedCharset("application", "json-seq", `UTF-8`)
  private val JsonObjectMaxSize = 100*1000  // TODO Sollte etwas größer als ein stdout-Batzen oder eine Variablenmenge sein

  val JsonSeqStreamSupport: JsonEntityStreamingSupport = {
    val rs = ByteString(Ascii.RS)
    val lf = ByteString(Ascii.LF)
    EntityStreamingSupport
      .json(maxObjectLength = JsonObjectMaxSize)
      .withContentType(ContentType(`application/json-seq`))
      .withParallelMarshalling(parallelism = sys.runtime.availableProcessors, unordered = false)
      .withFramingRenderer(Flow[ByteString].map(rs ++ _ ++ lf))
  }

  val NonEmptyEventSeqJsonStreamingSupport: JsonEntityStreamingSupport =
    EntityStreamingSupport
      .json(maxObjectLength = JsonObjectMaxSize)
      .withParallelMarshalling(parallelism = sys.runtime.availableProcessors, unordered = false)
      .withFramingRenderer(Flow[ByteString].intersperse(
        ByteString("""{"TYPE":"NonEmpty","stampeds":["""),
        ByteString(","),
        ByteString("]}")))

  def closeableIteratorToAkkaSource[A](iterator: CloseableIterator[A])(implicit scheduler: Scheduler): Source[A, NotUsed] =
    monixObservableToAkkaSource(closeableIteratorToObservable(iterator))
    // How to close without Monix??? Source.fromIterator(() ⇒ iterator).__onTerminate__(iterator.close()

  def monixObservableToAkkaSource[A](observable: Observable[A])(implicit scheduler: Scheduler): Source[A, NotUsed] =
    Source.fromPublisher(observable.toReactivePublisher(scheduler))

  def closeableIteratorToObservable[A](iterator: CloseableIterator[A])(implicit scheduler: Scheduler): Observable[A] =
    Observable.fromIterator(iterator).doOnTerminateEval(_ ⇒ Task.eval(iterator.close()))
}
