package js7.proxy.servlets

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import jakarta.servlet.ServletException
import jakarta.servlet.http.HttpServletResponse.{SC_INTERNAL_SERVER_ERROR, SC_OK, SC_SERVICE_UNAVAILABLE}
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.log.Logger
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.:=
import js7.common.metrics.MetricsProvider.PrometheusContentType
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.proxy.ControllerApi
import js7.proxy.servlets.ProxyMetricsServlet.*
import org.apache.pekko.util.ByteString
import scala.util.control.NonFatal

//??? @WebServlet(urlPatterns = Array("/metrics"))
final class ProxyMetricsServlet extends HttpServlet:

  private val isInUse = Atomic(false)

  @throws[ServletException]
  @throws[IOException]
  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit =
    try
      // Jakarta and Jetty both don't seem to offer a content-type negotiation via API ???
      ControllerApi.singleton match
        case None =>
          respond(response, SC_SERVICE_UNAVAILABLE,
            s"No ${classOf[ControllerApi].getName} with a ProxyId has been started\n")

        case Some((proxyId, controllerApi, ioRuntime)) =>
          given IORuntime = ioRuntime
          responseWithMetrics(controllerApi, response)
            .awaitInfinite
    catch
      case t: (IOException | RuntimeException | Error) => throw t
      case t => throw new ServletException(t.toString, t)

  private def responseWithMetrics(controllerApi: ControllerApi, response: HttpServletResponse)
  : IO[Unit] =
    IO.defer:
      if isInUse.getAndSet(true) then
        IO.blocking:
          respond(response, SC_SERVICE_UNAVAILABLE, "Still processing a concurrent GET request\n")
      else
        controllerApi.metrics.flatMap:
          case Left(problem) =>
            IO.blocking:
              respond(response, SC_INTERNAL_SERVER_ERROR, s"$problem\n")

          case Right(stream) =>
            writeMetricsStream(response, stream)
        .handleErrorWith: t =>
          IO.blocking:
            logger.trace(s"💥 $t")
            try
              if !response.isCommitted then
                respond(response, SC_INTERNAL_SERVER_ERROR, s"$t\n")
              else
                response.getOutputStream.write(s"\n# ERROR: $t\n".getBytes(UTF_8))
            catch case NonFatal(t2) =>
              if t ne t2 then t.addSuppressed(t2)
              logger.error(t.toString, t2)
    .guarantee:
      IO:
        isInUse := false

  private def writeMetricsStream(response: HttpServletResponse, stream: fs2.Stream[IO, ByteString])
  : IO[Unit] =
    IO.defer:
      response.setStatus(SC_OK)
      response.setCharacterEncoding("UTF-8") //?
      response.setContentType(PrometheusContentType.toString)
      val out = response.getOutputStream
      stream.foreach: chunk =>
        IO.blocking:
          out.write(chunk.unsafeArray)
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
  private val logger = Logger[this.type]
