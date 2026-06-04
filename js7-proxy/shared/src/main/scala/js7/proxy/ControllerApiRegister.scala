package js7.proxy

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import js7.base.log.Logger
import js7.base.problem.Checked.RichCheckedF
import js7.base.utils.ScalaConcurrentHashMap
import js7.base.utils.Tests.isTest
import js7.common.metrics.MetricsProvider.{mergeMetricStreams, toMetricsStream, toPrometheuesErrorLines}
import js7.proxy.ControllerApiRegister.*
import js7.proxy.data.GroupAndProxyId
import org.apache.pekko.util.ByteString

/** ControllerApiRegister provides Prometheus metrics of each Engine (ControllerApi)
  * for the ProxyMetricsServlet used by JOC.
  */
final class ControllerApiRegister(groupAndProxyId: Option[GroupAndProxyId])
  (using val ioRuntime: IORuntime)
extends MetricsForServlet:

  private val register = ScalaConcurrentHashMap[ControllerApi, Unit]

  private val localMetrics: fs2.Stream[IO, ByteString] =
    groupAndProxyId match
      case None =>
        fs2.Stream.emits(Seq(ByteString("# ERROR No GroupAndProxyId\n")))

      case Some(groupAndProxyId) =>
        toMetricsStream()(
          groupAndProxyId.groupId -> groupAndProxyId.proxyId.toJs7ServerId)

  def add(controllerApi: ControllerApi): Unit =
    logger.debug(s"Add $controllerApi ")
    register.updateWith(controllerApi):
      case None =>
        Some(())

      case Some(_) =>
        val msg = "ControllerApi registered twice"
        logger.error(msg)
        if isTest then throw new IllegalStateException(msg)
        Some(())

  def remove(controllerApi: ControllerApi): Unit =
    if register.remove(controllerApi).isDefined then
      logger.debug(s"Removed $controllerApi")

  def metrics(deep: Boolean): Stream[IO, ByteString] =
    if deep then
      mergeMetricStreams(localMetrics :: engineStreams)
    else
      localMetrics

  private def engineStreams: List[Stream[IO, ByteString]] =
    register.keys.toList.map: controllerApi =>
      Stream.force:
        controllerApi.metrics(deep = true) // Read Engine metrics
          .handleProblem: problem =>
            Stream.emit(ByteString(toPrometheuesErrorLines(problem.toString)))
      .handleErrorWith: throwable =>
        Stream.emit(ByteString(toPrometheuesErrorLines(throwable.toString)))

  override def toString = s"ControllerApiRegister(${register.size} ControllerApis)"


object ControllerApiRegister:
  private val logger = Logger[this.type]
