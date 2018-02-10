package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.{HttpResponse, ResponseEntity}
import akka.stream.Materializer
import akka.util.ByteString
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
object AkkaHttpClientUtils {

  implicit class RichResponseEntity(val underlying: ResponseEntity) extends AnyVal {
    // TODO Fail if Content-Type is not a (UTF-8 or other) String.
    // TODO Parameter maxLength to truncateWithEllipsis.
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8StringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[String] =
      byteStringFuture map (_.utf8String)  // Possible OutOfMemoryError

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteStringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[ByteString] =
      underlying.dataBytes.runFold(ByteString.empty)(_ ++ _)  // Possible OutOfMemoryError
  }

  implicit class RichHttpResponse(val underlying: HttpResponse) extends AnyVal {
    /**
      * Returns the HttpResponse content interpreted as UTF-8, ignoring any Content-Type.
      * May return a very big String.
      */
    def utf8StringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[String] =
      underlying.entity.utf8StringFuture

    /**
      * Returns the HttpResponse content as a `Future[ByteString]`.
      * May return a very big ByteString.
      */
    def byteStringFuture(implicit mat: Materializer, ec: ExecutionContext): Future[ByteString] =
      underlying.entity.byteStringFuture
  }
}
