package js7.proxy.javaapi

import cats.effect.ResourceIO
import cats.effect.unsafe.IORuntime
import java.time.Instant
import js7.base.log.LogLevel
import js7.base.utils.CatsUtils.Nel
import js7.controller.client.HttpControllerApi
import js7.data_for_java.reactor.ReactorConverters.asFlux
import reactor.core.publisher.Flux
import reactor.core.scheduler.Schedulers as ReactorSchedulers

final class JEngineLog(jProxy: JControllerProxy, controllerApis: Nel[HttpControllerApi])
  (using IORuntime):

  def currentLog(logLevel: LogLevel): Flux[String] =
    fs2.Stream.force:
      controllerApis.head.getLogLines(logLevel)
    .asFlux

  def logSection(logLevel: LogLevel, start: Instant, lines: Int): Flux[String] =
    fs2.Stream.force:
      controllerApis.head.getLogLines(logLevel, start = start, lines = lines)
    .asFlux
    .publishOn(ReactorSchedulers.fromExecutor(JResource.ourCommonPool))


//private def logSection(logLevel: LogLevel, start: Instant | LogPosition, lines: Int)
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

  def resource(jProxy: JControllerProxy)(using IORuntime): ResourceIO[JEngineLog] =
    jProxy.api.asScala.apisResource.map: apis =>
      JEngineLog(jProxy, apis)


  //private final case class LogPosition(logFileName: String, position: Long)
  //
  //private final case class LogLine(position: LogPosition, line: String)
