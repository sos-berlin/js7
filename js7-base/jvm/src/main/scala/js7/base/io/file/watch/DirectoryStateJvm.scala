package js7.base.io.file.watch

import java.nio.file.{Files, Path}
import js7.base.metering.CallMeter
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.JavaCollections.syntax.*
import js7.base.utils.ScalaUtils.syntax.*

object DirectoryStateJvm:

  private val meterReadDirectory = CallMeter("DirectoryStateJvm.readDirectory")

  def readDirectory(directory: Path, matches: Path => Boolean = _ => true): DirectoryState =
    meterReadDirectory:
      DirectoryState:
        autoClosing(Files.list(directory)):
          _.asScala
            .flatMap: file =>
              file.startsWith(directory) ? // Ignore silently alien paths
                directory.relativize(file)
            .filter: relativePath =>
              val s = relativePath.toString
              s != "." && s != ".." && matches(relativePath)
            .map: path =>
              path -> DirectoryState.Entry(path)
            .toMap
