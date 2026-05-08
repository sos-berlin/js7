package js7.proxy.servlets

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import jakarta.servlet.http.HttpServletResponse.{SC_OK, SC_SERVICE_UNAVAILABLE}
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import jakarta.servlet.{ServletException, ServletOutputStream}
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.:=
import js7.common.metrics.MetricsProvider
import js7.common.metrics.MetricsProvider.{PrometheusContentType, toPrometheuesErrorLines}
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.proxy.ControllerApi
import js7.proxy.ControllerApi.Registered
import js7.proxy.servlets.ProxyMetricsServlet.*
import org.apache.pekko.util.ByteString

//@WebServlet(urlPatterns = Array("/metrics"))
final class ProxyMetricsServlet extends HttpServlet:

  private val isInUse = Atomic(false)

  @throws[ServletException]
  @throws[IOException]
  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit =
    // Jakarta and Jetty both don't seem to offer a content-type negotiation via API ?
    try
      // Each Proxy corresponds to a separate Engine (with own ControllerId)
      val registeredControllerApis = ControllerApi.registeredControllerApis
      if registeredControllerApis.isEmpty then
        respond(response, SC_SERVICE_UNAVAILABLE,
          s"No ${classOf[ControllerApi].getName} with a ProxyId has been started\n")
      else if isInUse.getAndSet(true) then
        respond(response, SC_SERVICE_UNAVAILABLE, "Still processing a concurrent GET request\n")
      else
        try
          response.setStatus(SC_OK)
          response.setCharacterEncoding("UTF-8")
          response.setContentType(PrometheusContentType.toString)

          given IORuntime = registeredControllerApis.head.ioRuntime
          respondWithMetricStream(
            getEngineStreams(registeredControllerApis),
            response.getOutputStream
          ).awaitInfinite
        finally
          isInUse := false
    catch
      case t: (IOException | RuntimeException | Error) => throw t
      case t => throw new ServletException(t.toString, t)

  private def getEngineStreams(registeredControllerApis: Seq[Registered])(using IORuntime)
  : Seq[Stream[IO, ByteString]] =
    registeredControllerApis.map:
      case (_, controllerApi, ioRuntime) =>
        if ioRuntime != summon[IORuntime] then
          logger.error:
            s"$controllerApi has a different IORuntime (JProxyContext) than the others"
          Stream.empty
        else
          Stream.force:
            controllerApi.metrics // Read Engine metrics
              .handleProblem: problem =>
                Stream.emit(ByteString(toPrometheuesErrorLines(problem.toString)))
          .handleErrorWith: throwable =>
            Stream.emit(ByteString(toPrometheuesErrorLines(throwable.toString)))

  private def respondWithMetricStream(streams: Seq[Stream[IO, ByteString]], out: ServletOutputStream)
  : IO[Unit] =
    MetricsProvider.mergeMetricStreams(streams)
      .map(_.toChunk).unchunks[Byte].chunkLimit(httpChunkSize)
      .foreach: chunk =>
        IO.blocking:
          out.write(chunk.unsafeArray)
          // Java EE 11: out.write(chunk.toByteBuffer)
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
  private val httpChunkSize = 1024 * 1024
