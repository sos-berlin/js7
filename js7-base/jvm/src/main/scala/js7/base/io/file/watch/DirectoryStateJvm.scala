package js7.base.io.file.watch

import java.nio.file.{Files, Path}
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.JavaCollections.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.Deadline.now

object DirectoryStateJvm:

  private val logger = Logger[this.type]

  def readDirectory(directory: Path, matches: Path => Boolean = _ => true): DirectoryState =
    val since = now
    val directoryState = DirectoryState(
      autoClosing(Files.list(directory))(_
        .asScala
        .flatMap(file =>
          file.startsWith(directory) ?  // Ignore silently alien paths
            directory.relativize(file))
        .filter { relativePath =>
          val s = relativePath.toString
          s != "." && s != ".." && matches(relativePath)
        }
        .map(path => path -> DirectoryState.Entry(path))
        .toMap))
    logger.trace(
      s"readDirectory '$directory' => ${directoryState.fileToEntry.size} files in ${since.elapsed.pretty}")
    directoryState
