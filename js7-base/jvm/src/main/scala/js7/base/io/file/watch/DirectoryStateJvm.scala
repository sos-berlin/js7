package js7.base.io.file.watch

import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.JavaCollections.syntax.*
import js7.base.utils.ScalaUtils.syntax.*

object DirectoryStateJvm:

  private val logger = Logger[this.type]

  def readDirectory(directory: Path, matches: Path => Boolean = _ => true): DirectoryState =
    logger.traceCallWithResult("readDirectory", directory,
      result = o => s"${o.fileToEntry.size} files",
      body =
        DirectoryState(
          autoClosing(Files.list(directory))(_
            .asScala
            .flatMap(file =>
              file.startsWith(directory) ? // Ignore silently alien paths
                directory.relativize(file))
            .filter: relativePath =>
              val s = relativePath.toString
              s != "." && s != ".." && matches(relativePath)
            .map(path => path -> DirectoryState.Entry(path))
            .toMap)))
