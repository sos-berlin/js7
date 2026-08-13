package js7.proxy.javaapi.log

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import java.time.Instant
import java.util.List as JList
import javax.annotation.Nonnull
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.controller.client.HttpControllerApi
import js7.data.node.Js7ServerId
import js7.data_for_java.reactor.ReactorConverters.*
import js7.proxy.javaapi.JControllerProxy
import js7.proxy.javaapi.log.JEngineLog.*
import js7.proxy.javaapi.log.{JLogIndex, JLogSelection}
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*

final class JEngineLog private(
  jProxy: JControllerProxy,
  primaryHttpControllerApi: HttpControllerApi,
  serverId: Js7ServerId,
  logLevel: LogLevel)
  (using IORuntime)
extends JLogIndex:

  private lazy val activeHttpControllerApiResource =
    jProxy.api.asScala.activeHttpControllerApiResource
  private lazy val backupHttpControllerApiResource =
    jProxy.api.asScala.backupHttpControllerApiResource

  @Nonnull
  def byteLogLineFlux(begin: Instant, logSelection: JLogSelection): Flux[JList[Array[Byte]]] =
    logLineStream(begin, logSelection, _.unsafeArray)
      .asFlux

  @Nonnull
  def byteLogLineFlux(key: LogLineKey, logSelection: JLogSelection): Flux[JList[Array[Byte]]] =
    logLineStream(key, logSelection, _.unsafeArray)
      .asFlux

  @Nonnull
  def stringLogLineFlux(begin: Instant, logSelection: JLogSelection): Flux[JList[String]] =
    logLineStream(begin, logSelection, _.utf8String)
      .asFlux

  @Nonnull
  def keyedByteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]] =
    keyedByteLogLineStream(begin, logSelection, identity)
      .asFlux

  @Nonnull
  def keyedByteLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]] =
    keyedByteLogLineStream(begin, logSelection, identity)
      .asFlux

  @Nonnull
  def keyedLogLineFlux(begin: Instant, logSelection: JLogSelection): Flux[JList[KeyedLogLine]] =
    keyedLogLineStream(begin, logSelection)
      .asFlux

  @Nonnull
  def keyedLogLineFlux(key: LogLineKey, logSelection: JLogSelection): Flux[JList[KeyedLogLine]] =
    keyedLogLineStream(key, logSelection)
      .asFlux

  private[javaapi] def logLineStream[R](
    begin: Instant | LogLineKey,
    logSelection: JLogSelection,
    convert: fs2.Chunk[Byte] => R)
  : fs2.Stream[IO, JList[R]] =
    logger.traceStream(s"logLineStream Stream"):
      fs2.Stream.force:
        serverId match
          case Js7ServerId.Controller.Primary =>
            primaryHttpControllerApi.getLogLines(logLevel, begin, logSelection.asScala)
          case Js7ServerId.Controller.Backup =>
            backupHttpControllerApiResource.use:
              _.getLogLines(logLevel, begin, logSelection.asScala)
          case Js7ServerId.Subagent(subagentId) =>
            activeHttpControllerApiResource.use:
              _.getLogLines(logLevel, begin, logSelection.asScala,
                subagentId = Some(subagentId))
          case _: (Js7ServerId.Proxy | Js7ServerId.Provider) => IO.pure(fs2.Stream.empty)
      .map(convert)
      .chunks
      .map(_.asJava)

  private[javaapi] def keyedByteLogLineStream[R](
    begin: Instant | LogLineKey,
    logSelection: JLogSelection,
    convert: KeyedByteLogLine => R)
  : fs2.Stream[IO, JList[R]] =
    logger.traceStream(s"logLineStream Stream"):
      fs2.Stream.force:
        serverId match
          case Js7ServerId.Controller.Primary =>
            primaryHttpControllerApi.getKeyedByteLogLines(logLevel, begin, logSelection.asScala)
          case Js7ServerId.Controller.Backup =>
            backupHttpControllerApiResource.use:
              _.getKeyedByteLogLines(logLevel, begin, logSelection.asScala)
          case Js7ServerId.Subagent(subagentId) =>
            activeHttpControllerApiResource.use:
              _.getKeyedByteLogLines(logLevel, begin, logSelection.asScala,
                subagentId = Some(subagentId))
          case _: (Js7ServerId.Proxy | Js7ServerId.Provider) => IO.pure(fs2.Stream.empty)
      .map(convert)
      .chunks
      .map(_.asJava)

  private[javaapi] def keyedLogLineStream(begin: Instant | LogLineKey, logSelection: JLogSelection)
  : fs2.Stream[IO, JList[KeyedLogLine]] =
    logger.traceStream(s"keyedLogLineFlux Stream"):
      fs2.Stream.force:
        serverId match
          case Js7ServerId.Controller.Primary =>
            primaryHttpControllerApi.getKeyedLogLines(logLevel, begin, logSelection.asScala)
          case Js7ServerId.Controller.Backup =>
            backupHttpControllerApiResource.use:
              _.getKeyedLogLines(logLevel, begin, logSelection.asScala)
          case Js7ServerId.Subagent(subagentId) =>
            activeHttpControllerApiResource.use:
              _.getKeyedLogLines(logLevel, begin, logSelection.asScala,
                subagentId = Some(subagentId))
          case _: (Js7ServerId.Proxy | Js7ServerId.Provider) => IO.pure(fs2.Stream.empty)
    .chunks.map(_.asSeq.asJava)


object JEngineLog:
  private val logger = Logger[this.type]

  def resource(jProxy: JControllerProxy, serverId: Js7ServerId, logLevel: LogLevel)(using IORuntime)
  : ResourceIO[JEngineLog] =
    logger.traceResource("JEngineLog.resource"):
      for
        primaryApi <- jProxy.api.asScala.primaryHttpControllerApiResource
      yield
        JEngineLog(jProxy, primaryApi, serverId, logLevel)
