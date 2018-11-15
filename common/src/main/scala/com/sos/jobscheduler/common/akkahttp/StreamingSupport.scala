package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.{ContentType, MediaType}
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import com.google.common.base.Ascii
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
        ByteString("""{"TYPE":"NonEmpty","stamped":["""),
        ByteString(","),
        ByteString("]}")))

  implicit final class AkkaObservable[A](private val underlying: Observable[A]) extends AnyVal {
    def toAkkaSource(implicit scheduler: Scheduler): Source[A, NotUsed] =
      Source.fromPublisher(underlying.toReactivePublisher(scheduler))
  }
}

