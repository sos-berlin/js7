package js7.base.log.reader.recompressors

import cats.effect.{IO, Resource, ResourceIO}
import com.typesafe.config.Config
import java.io.{FileOutputStream, InputStream, OutputStream}
import java.nio.file.Path
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.convert.As.StringAsBoolean
import js7.base.log.reader.LogFileIndex.LogWriter

private[reader] trait Recompressor:

  def decompressingInputStream(in: InputStream): InputStream

  def toLogWriter(out: OutputStream): ResourceIO[LogWriter]

  final def toLogWriter(file: Path): ResourceIO[LogWriter] =
    Resource.fromAutoCloseable:
      IO.blocking:
        new FileOutputStream(file.toFile)
    .flatMap:
      toLogWriter

  def label: String =
    if this eq PlainRecompressor then
      "Decompressed and indexed"
    else
      "Recompressed and indexed"


private[reader] object Recompressor:
  def fromConfig(config: Config): Recompressor =
    config.as[String]("js7.log.index.recompress") match
      case "gzip" => GzipRecompressor
      case "deflater" => DeflaterRecompressor
      case string =>
        if StringAsBoolean(string) then
          DeflaterRecompressor // Faster than GzipRecompressor
        else
          PlainRecompressor
