package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource, ResourceIO}
import com.typesafe.config.Config
import fs2.Chunk
import java.io.{FileOutputStream, InputStream, OutputStream}
import java.nio.file.Path
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.io.{SeekableInputStream, SeekableOutputStream}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogIndexConf
import js7.base.log.{LogLevel, Logger}
import js7.base.system.JavaServiceProviders

trait Recompressor:

  def isFast: Boolean =
    false

  def findRecompressor(name: String): Option[Recompressor]

  def decompressingInputStream(in: InputStream)(using LogIndexConf): SeekableInputStream

  protected def newCompressiongOutputStream(out: OutputStream)(using LogIndexConf)
  : SeekableOutputStream

  final def toLogWriter(file: Path)(using LogIndexConf): ResourceIO[LogWriter] =
    Resource.fromAutoCloseable:
      IO.blocking:
        new FileOutputStream(file.toFile)
    .flatMap:
      toLogWriter

  final def toLogWriter(out: OutputStream)(using LogIndexConf): Resource[IO, LogWriter] =
    Resource.fromAutoCloseable:
      IO.blocking:
        newCompressiongOutputStream(out)
    .flatMap: (out: SeekableOutputStream) =>
      Resource.fromAutoCloseable:
        IO:
          new LogWriter with AutoCloseable:
            private val buf = new Array[Byte](512)
            private var _bytePosition = 0L

            def write(chunk: Chunk[Byte]): Unit =
              out.write(chunk.toArray)
              _bytePosition += chunk.size

            def position: Long =
              _bytePosition

            def markOpaquePos() =
              out.markOpaquePos()

            def close() =
              out.close()


object Recompressor:
  private val logger = Logger[this.type]
  val default = DeflateRecompressor // Faster than GzipRecompressor
  private var unknownRecompressors = Set.empty[String]

  val knownRecompressors: Seq[Recompressor] =
    Seq(PlainRecompressor, /*GzipRecompressor test fails,*/ DeflateRecompressor)

  private lazy val javaServices: Seq[Recompressor] =
    JavaServiceProviders.findJavaServices[Recompressor]

  def fromConfig(config: Config): Recompressor =
    val keyName = "js7.log.index.recompress"
    val name = config.as[String](keyName)
    (knownRecompressors.iterator ++ javaServices.iterator)
      .flatMap: recompressor =>
        recompressor.findRecompressor(name)
      .take(1).toList.headOption.getOrElse:
        val logLevel = if unknownRecompressors(name) then LogLevel.Debug else LogLevel.Error
        unknownRecompressors += name
        logger.log(logLevel, s"Unknown Recompressor $keyName=$name, falling back to $default")
        default
