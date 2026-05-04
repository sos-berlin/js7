package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource, ResourceIO}
import com.typesafe.config.Config
import java.io.{FileOutputStream, InputStream, OutputStream}
import java.nio.file.Path
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.log.Logger
import js7.base.log.reader.LogFileIndex.LogWriter
import js7.base.system.JavaServiceProviders

trait Recompressor:

  def findRecompressor(name: String): Option[Recompressor]

  def decompressingInputStream(in: InputStream): InputStream

  def toLogWriter(out: OutputStream): ResourceIO[LogWriter]

  final def toLogWriter(file: Path): ResourceIO[LogWriter] =
    Resource.fromAutoCloseable:
      IO.blocking:
        new FileOutputStream(file.toFile)
    .flatMap:
      toLogWriter


private[reader] object Recompressor:
  private val logger = Logger[this.type]
  private val default = DeflateRecompressor // Faster than GzipRecompressor

  private val knownRecompressors: Seq[Recompressor] =
    Seq(PlainRecompressor, GzipRecompressor, DeflateRecompressor)

  private lazy val javaServices: Seq[Recompressor] =
    JavaServiceProviders.findJavaServices[Recompressor]

  def fromConfig(config: Config): Recompressor =
    val keyName = "js7.log.index.recompress"
    val name = config.as[String](keyName)
    (knownRecompressors.iterator ++ javaServices.iterator)
      .flatMap: recompressor =>
        recompressor.findRecompressor(name)
      .take(1).toList.headOption.getOrElse:
        logger.error(s"Unknown Recompressor $keyName=$name, falling back to $default")
        default
