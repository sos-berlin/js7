package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import cats.syntax.foldable.*
import java.time.Instant
import javax.annotation.Nonnull
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.{KeyedLogLine, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.utils.CatsUtils.Nel
import js7.controller.client.HttpControllerApi
import js7.data.node.{Js7ServerId, NodeId}
import js7.data_for_java.reactor.ReactorConverters.*
import js7.proxy.javaapi.JEngineLog.*
import js7.proxy.javaapi.log.JLogSelection
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*

final class JEngineLog(
  jProxy: JControllerProxy,
  controllerApis: Nel[HttpControllerApi],
  serverId: Js7ServerId)
  (using IORuntime):

  private val primaryControllerApi: HttpControllerApi =
    controllerApis.head

  private val backupControllerApi: HttpControllerApi =
    controllerApis.get(1).getOrElse(primaryControllerApi)

  def currentLog(logLevel: LogLevel): Flux[Array[Byte]] =
    fs2.Stream.force:
      serverId match
        case Js7ServerId.Controller.Primary =>
          primaryControllerApi.getNewLogLines(logLevel)
        case Js7ServerId.Controller.Backup =>
          backupControllerApi.getNewLogLines(logLevel)
        case Js7ServerId.Subagent(subagentId) =>
          activeControllerApi.getNewLogLines(logLevel, subagentId = Some(subagentId))
        case _: (Js7ServerId.Proxy | Js7ServerId.Provider) => IO.pure(fs2.Stream.empty)
    .map(_.toArray) // Copy, or has Java an immutable array?
    .asFlux

  @Nonnull
  def keyedLogLineFlux(
    logLevel: LogLevel,
    begin: Instant,
    logSelection: JLogSelection)
  : Flux[java.util.List[KeyedLogLine]] =
    keyedLogLineStream(logLevel, begin, logSelection)
      .asFlux

  /** Read log lines beginning after the line denoted by `key`. */
  @Nonnull
  def keyedLogLineFlux(
    logLevel: LogLevel,
    key: LogLineKey,
    logSelection: JLogSelection)
  : Flux[java.util.List[KeyedLogLine]] =
    keyedLogLineStream(logLevel, key, logSelection)
      .asFlux

  private[javaapi] def keyedLogLineStream(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    logSelection: JLogSelection)
  : fs2.Stream[IO, java.util.List[KeyedLogLine]] =
    logger.traceStream(s"keyedLogLineFlux Stream"):
      fs2.Stream.force:
        serverId match
          case Js7ServerId.Controller.Primary =>
            primaryControllerApi.getKeyedLogLines(logLevel, begin = begin, logSelection.toScala)
          case Js7ServerId.Controller.Backup =>
            backupControllerApi.getKeyedLogLines(logLevel, begin = begin, logSelection.toScala)
          case Js7ServerId.Subagent(subagentId) =>
            activeControllerApi.getKeyedLogLines(logLevel, begin = begin, logSelection.toScala,
              subagentId = Some(subagentId))
          case _: (Js7ServerId.Proxy | Js7ServerId.Provider) => IO.pure(fs2.Stream.empty)
    .chunks.map(_.asSeq.asJava)

  /** Read log lines as Array[Byte] beginning with `begin`. */
  @Nonnull
  def byteLogLineFlux(
    logLevel: LogLevel,
    begin: Instant,
    logSelection: JLogSelection)
  : Flux[java.util.List[Array[Byte]]] =
    logLineStream(logLevel, begin, logSelection, _.unsafeArray)
      .asFlux

  /** Read log lines as Array[Byte] beginning after the line denoted by `key`. */
  @Nonnull
  def byteLogLineFlux(
    logLevel: LogLevel,
    key: LogLineKey,
    logSelection: JLogSelection)
  : Flux[java.util.List[Array[Byte]]] =
    logLineStream(logLevel, key, logSelection, _.unsafeArray)
      .asFlux

  /** Read log lines from `begin`. */
  @Nonnull
  def stringLogLineFlux(
    logLevel: LogLevel,
    begin: Instant,
    logSelection: JLogSelection)
  : Flux[java.util.List[String]] =
    logLineStream(logLevel, begin, logSelection, _.utf8String)
      .asFlux

  /** Read log lines beginning after the line denoted by `key`. */
  @Nonnull
  def stringLogLineFlux(
    logLevel: LogLevel,
    key: LogLineKey,
    logSelection: JLogSelection)
  : Flux[java.util.List[String]] =
    logLineStream(logLevel, key, logSelection, _.utf8String)
      .asFlux

  private[javaapi] def logLineStream[R](
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    logSelection: JLogSelection,
    convert: fs2.Chunk[Byte] => R)
  : fs2.Stream[IO, java.util.List[R]] =
    logger.traceStream(s"logLineStream Stream"):
      fs2.Stream.force:
        serverId match
          case Js7ServerId.Controller.Primary =>
            primaryControllerApi.getLogLines(logLevel, begin = begin, logSelection.toScala)
          case Js7ServerId.Controller.Backup =>
            backupControllerApi.getLogLines(logLevel, begin = begin, logSelection.toScala)
          case Js7ServerId.Subagent(subagentId) =>
            activeControllerApi.getLogLines(logLevel, begin = begin, logSelection.toScala,
              subagentId = Some(subagentId))
          case _: Js7ServerId.Proxy => IO.pure(fs2.Stream.empty)
      .map(convert)
      .chunks
      .map(_.asJava)

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
