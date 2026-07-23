package js7.base.log.reader

import java.nio.file.Path
import js7.base.log.LogLevel

object LogUtils:

  private[reader] def isOurFilenameAnyLevel(prefix: String): Path => Boolean =
    val currentErrorFilename = prefix + toFilenamePart(LogLevel.Error) + ".log"
    val currentInfoFilename = prefix + toFilenamePart(LogLevel.Info) + ".log"
    val currentDebugFilename = prefix + toFilenamePart(LogLevel.Debug) + ".log"
    val compressedErrorPrefix = prefix + toFilenamePart(LogLevel.Error) + "-"
    val compressedInfoPrefix = prefix + toFilenamePart(LogLevel.Info) + "-"
    val compressedDebugPrefix = prefix + toFilenamePart(LogLevel.Debug) + "-"

    filename =>
      val name = filename.getFileName.toString

      def isCurrentFile =
        name == currentErrorFilename ||
          name == currentInfoFilename ||
          name == currentDebugFilename

      def isCompressedFile: Boolean =
        name.endsWith(".log.gz") && (
          name.startsWith(compressedErrorPrefix) ||
            name.startsWith(compressedInfoPrefix) ||
            name.startsWith(compressedDebugPrefix))

      isCurrentFile || isCompressedFile

  def isOurFilename(prefix: String, logLevel: LogLevel): Path => Boolean =
    val currentLogFile = prefix + toFilenamePart(logLevel) + ".log"
    val compressedPrefix = prefix + toFilenamePart(logLevel) + "-"

    filename =>
      val name = filename.getFileName.toString
      name == currentLogFile ||
        name.endsWith(".log.gz") && name.startsWith(compressedPrefix)

  private def toFilenamePart(logLevel: LogLevel): String =
    logLevel match
      case LogLevel.Error => "-error"
      case LogLevel.Info => ""
      case LogLevel.Debug => "-debug"
      case _ => throw IllegalArgumentException(s"Unsupported log level $logLevel")
