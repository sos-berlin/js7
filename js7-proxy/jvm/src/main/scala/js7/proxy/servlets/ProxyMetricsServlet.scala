package js7.proxy.servlets

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, SyncIO}
import jakarta.servlet.ServletException
import jakarta.servlet.http.HttpServletResponse.{SC_OK, SC_SERVICE_UNAVAILABLE}
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.convert.As.StringAsBoolean
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.Logger
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.{+=, whenInUse}
import js7.common.http.{HttpMXBean, HttpMXBeanUtils}
import js7.common.metrics.MetricsProvider.PrometheusContentType
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.proxy.MetricsForServlet
import js7.proxy.javaapi.JProxyContext
import js7.proxy.servlets.ProxyMetricsServlet.*
import org.apache.pekko.util.ByteString

//@WebServlet(urlPatterns = Array("/metrics"))
final class ProxyMetricsServlet(toMetricsForServlet: () => Option[MetricsForServlet])
  extends HttpServlet:

  /** Use this constructor, when this Servlet is registered programmatically after
    * [[JProxyContext]] has been established.
    *
    * No call to [[JProxyContext#makeSingleton()]] needed, no static variable is used.
    */
  def this(jProxyContext: JProxyContext) =
    this(() => Some(jProxyContext.metricsForServlet))

  /** Parameterless constructor, when this Servlet is registered via web xml.
    *
    * A call to [[JProxyContext#makeSingleton()]] gives this Servlet access to
    * [[MetricsForServlet]] (i.e., metrics of this JVM and the observed JS7 Engines)
    * through a static variable.
    */
  def this() =
    this(() => JProxyContext.metricsForServlet)


  private val isInUse = Atomic(false)

  logger.debug("ProxyMetricsServlet")

  @throws[ServletException]
  @throws[IOException]
  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit =
    // Jakarta and Jetty both don't seem to offer a content-type negotiation via API ?
    val deep = request.getParameterValues("deep").forall(StringAsBoolean)
    HttpMXBeanUtils.clientRequestResource[SyncIO].surround:
      isInUse.whenInUse:
        SyncIO:
          respond(response, SC_SERVICE_UNAVAILABLE, "Still processing a concurrent GET request\n")
      .otherwiseUse:
        SyncIO:
          toMetricsForServlet() match
            case None =>
              respond(response, SC_SERVICE_UNAVAILABLE, "No JControllerContext singleton here\n")

            case Some(metricsForServlet) =>
              given IORuntime = metricsForServlet.ioRuntime
              doGet2(request, response, metricsForServlet.metrics(deep = deep))
                .handleErrorWith:
                  case t: (IOException | RuntimeException) => IO.raiseError(t)
                  case t: Exception => IO.raiseError(new ServletException(t.toString, t))
                  case t => IO.raiseError(t)
                .awaitInfinite
    .run()

  @throws[ServletException]
  @throws[IOException]
  private def doGet2(request: HttpServletRequest, response: HttpServletResponse,
    metrics: fs2.Stream[IO, ByteString])
  : IO[Unit] =
    IO.defer:
      // Jakarta and Jetty both don't seem to offer a content-type negotiation via API ?
      response.setStatus(SC_OK)
      response.setCharacterEncoding("UTF-8")
      response.setContentType(PrometheusContentType.toString)

      val out = response.getOutputStream
      metrics
        .map(_.toChunk).unchunks[Byte].chunkLimit(httpChunkSize)
        .foreach: chunk =>
          IO.blocking:
            // Java EE 11: out.write(chunk.toByteBuffer)
            out.write(chunk.unsafeArray)
            HttpMXBean.Bean.serverSentChunksTotal += 1
            HttpMXBean.Bean.serverSentByteTotal += chunk.size
        .compile.drain

  private def respond(response: HttpServletResponse, status: Int, message: String): Unit =
    Logger.traceCall("response", (status, message)):
      response.setStatus(status)
      response.setContentType("text/plain")
      response.setCharacterEncoding("UTF-8")
      val out = response.getOutputStream
      out.write(message.getBytes(UTF_8))
      if !message.endsWith("\n") then
        out.write("\n".getBytes(UTF_8))


object ProxyMetricsServlet:
  Logger.dontInitialize()
  private val logger = Logger[this.type]
  private val httpChunkSize = 1024 * 1024
