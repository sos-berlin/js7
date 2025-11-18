package js7.service.prometheus

import io.prometheus.metrics.exporter.common.{PrometheusHttpExchange, PrometheusHttpRequest, PrometheusHttpResponse}
import java.io.{ByteArrayOutputStream, IOException, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import org.apache.pekko.http.scaladsl.model.HttpRequest
import org.apache.pekko.util.ByteString
import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional

// Not used !!!
private[prometheus] object PrometheusJmxAdapterForHttp:
  private val logger = Logger[this.type]

  final class PekkoPromtheusExchange(request: HttpRequest) extends PrometheusHttpExchange:
    private val response = new Response

    def getRequest: PrometheusHttpRequest =
      new PrometheusHttpRequest:
        def getQueryString =
          request.uri.queryString(UTF_8) getOrElse ""

        def getHeaders(name: String): java.util.Enumeration[String] =
          request.getHeader(name).toScala.fold(EmptyEnumeration): header =>
            SingleEnumeration(header.value)

        def getMethod =
          request.method.value

        def getRequestPath =
          request.uri.path.toString

    def getResponse: PrometheusHttpResponse =
      response

    def handleException(e: IOException): Unit =
      logger.error(e.toStringWithCauses, e)

    def handleException(e: RuntimeException): Unit =
      logger.error(e.toStringWithCauses, e)

    def close(): Unit =
      ()

    def metricsByteString: ByteString =
      response.byteString


  private final class Response extends PrometheusHttpResponse:
    private var statusCode = 0
    private var contentLength = 0
    private val headers = mutable.Map.empty[String, String]
    private val outputStream = new ByteArrayOutputStream(1024)

    def setHeader(name: String, value: String): Unit =
      headers.put(name, value)

    def sendHeadersAndGetBody(statusCode: Int, contentLength: Int): OutputStream =
      this.statusCode = statusCode
      this.contentLength = contentLength
      outputStream

    def byteString: ByteString =
      ByteString.fromArrayUnsafe(outputStream.toByteArray)


  private object EmptyEnumeration extends java.util.Enumeration[String]:
    def hasMoreElements = false
    def nextElement = throw new NoSuchElementException


  private final class SingleEnumeration(string: String) extends java.util.Enumeration[String]:
    private var hasNext = true

    def hasMoreElements =
      hasNext

    def nextElement =
      if !hasNext then throw new NoSuchElementException
      hasNext = false
      string
