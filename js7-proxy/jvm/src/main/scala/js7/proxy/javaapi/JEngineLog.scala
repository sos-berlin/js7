package js7.proxy.javaapi

import cats.effect.ResourceIO
import cats.effect.unsafe.IORuntime
import java.time.Instant
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.utils.CatsUtils.Nel
import js7.controller.client.HttpControllerApi
import js7.data_for_java.reactor.ReactorConverters.asFlux
import reactor.core.publisher.Flux

final class JEngineLog(jProxy: JControllerProxy, controllerApis: Nel[HttpControllerApi])
  (using IORuntime):

  def currentLog(logLevel: LogLevel): Flux[Array[Byte]] =
    fs2.Stream.force:
      controllerApis.head.getLogLines(logLevel)
    .map(_.toArray) // Copy, or has Java an immutable array?
    .asFlux

  def logSection(logLevel: LogLevel, begin: Instant, lines: Long): Flux[java.util.List[Array[Byte]]] =
    fs2.Stream.force:
      controllerApis.head.getLogLines(logLevel, begin = begin, lines = lines)
    .map(_.toArray) // Copy, or has Java an immutable array?
    .chunks
    .asFlux
    .map(_.asJava)

  //private def keyedLogLineFlux(logLevel: LogLevel, start: Instant | LogPosition, lines: Long)
  //: CompletableFuture[Flux[String]] =
  //  ???

  //private def serverIdToApi(serverId: EngineServerId): HttpControllerApi =
  //  serverId match
  //    case serverId: ControllerServerId =>
  //      jProxy.clusterState match
  //        case ClusterState.Empty =>
  //          controllerApis.head
  //
  //        case clusterState: ClusterState.HasNodes =>
  //          controllerApis.toList
  //            .find(_.baseUri == clusterState.activeUri)
  //            .getOrElse:
  //              throw new IllegalStateException(
  //                s"No admission for $serverId ${clusterState.activeUri} ")
  //
  //    case _ => throw new IllegalArgumentException(s"Must be ControllerServerId: $serverId")


object JEngineLog:
  private val logger = Logger[this.type]

  def resource(jProxy: JControllerProxy)(using IORuntime): ResourceIO[JEngineLog] =
    logger.traceResource("JEngineLog.resource"):
      jProxy.api.asScala.apisResource.map: apis =>
        JEngineLog(jProxy, apis)


  //private final case class LogPosition(logFileName: String, position: Long)
  //
  //private final case class LogLine(position: LogPosition, line: String)
