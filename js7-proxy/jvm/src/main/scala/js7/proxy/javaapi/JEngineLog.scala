package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import cats.syntax.foldable.*
import java.time.Instant
import java.util.OptionalLong
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.{KeyedLogLine, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.utils.CatsUtils.Nel
import js7.controller.client.HttpControllerApi
import js7.data.node.{Js7ServerId, NodeId}
import js7.data_for_java.reactor.ReactorConverters.asFlux
import js7.proxy.javaapi.JEngineLog.*
import reactor.core.publisher.Flux
import scala.jdk.OptionConverters.*

final class JEngineLog(
  jProxy: JControllerProxy,
  controllerApis: Nel[HttpControllerApi],
  serverId: Js7ServerId)
  (using IORuntime):

  def currentLog(logLevel: LogLevel): Flux[Array[Byte]] =
    fs2.Stream.force:
      serverId match
        case Js7ServerId.Controller.Primary =>
          primaryControllerApi.getNewLogLines(logLevel)
        case Js7ServerId.Controller.Backup =>
          backupControllerApi.getNewLogLines(logLevel)
        case Js7ServerId.Subagent(subagentId) =>
          activeControllerApi.getNewLogLines(logLevel, subagentId = Some(subagentId))
    .map(_.toArray) // Copy, or has Java an immutable array?
    .asFlux

  def logSection(logLevel: LogLevel, begin: Instant, lines: OptionalLong)
  : Flux[java.util.List[Array[Byte]]] =
    logSection_(logLevel, begin, lines)
      .map(_.toArray) // Copy, or has Java an immutable array?
      .chunks
      .map(_.asJava)
      .asFlux

  def logSection(logLevel: LogLevel, begin: LogLineKey, lines: OptionalLong)
  : Flux[java.util.List[Array[Byte]]] =
    logSection_(logLevel, begin, lines)
      .map(_.toArray) // Copy, or has Java an immutable array?
      .chunks
      .map(_.asJava)
      .asFlux

  private[javaapi] def logSection_(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    lines: OptionalLong)
  : fs2.Stream[IO, fs2.Chunk[Byte]] =
    fs2.Stream.force:
      serverId match
        case Js7ServerId.Controller.Primary =>
          primaryControllerApi.getLogLines(logLevel, begin = begin, lines = lines.toScala)
        case Js7ServerId.Controller.Backup =>
          backupControllerApi.getLogLines(logLevel, begin = begin, lines = lines.toScala)
        case Js7ServerId.Subagent(subagentId) =>
          activeControllerApi.getLogLines(logLevel, begin = begin, lines = lines.toScala,
            subagentId = Some(subagentId))

  private[javaapi] def keyedLogLineStream(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    lines: OptionalLong)
  : fs2.Stream[IO, KeyedLogLine] =
    logger.traceStream(s"keyedLogLineFlux Stream"):
      fs2.Stream.force:
        serverId match
          case Js7ServerId.Controller.Primary =>
            primaryControllerApi.getKeyedLogLines(logLevel, begin = begin, lines = lines.toScala)
          case Js7ServerId.Controller.Backup =>
            backupControllerApi.getKeyedLogLines(logLevel, begin = begin, lines = lines.toScala)
          case Js7ServerId.Subagent(subagentId) =>
            activeControllerApi.getKeyedLogLines(logLevel, begin = begin, lines = lines.toScala,
              subagentId = Some(subagentId))

  private def primaryControllerApi: HttpControllerApi =
    controllerApis.head

  private def backupControllerApi: HttpControllerApi =
    controllerApis.get(1).getOrElse(primaryControllerApi)

  private def activeControllerApi: HttpControllerApi =
    if jProxy.clusterState.isEmptyOrActive(NodeId.primary) then
      primaryControllerApi
    else
      backupControllerApi


object JEngineLog:
  private val logger = Logger[this.type]

  def resource(jProxy: JControllerProxy, serverId: Js7ServerId)(using IORuntime)
  : ResourceIO[JEngineLog] =
    logger.traceResource("JEngineLog.resource"):
      jProxy.api.asScala.apisResource.map: apis =>
        JEngineLog(jProxy, apis, serverId)
