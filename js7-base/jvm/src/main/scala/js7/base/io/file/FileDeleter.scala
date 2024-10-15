package js7.base.io.file

import java.nio.file.Files.{deleteIfExists, exists}
import java.nio.file.Path
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.control.NonFatal

object FileDeleter:
  private val logger = Logger[this.type]

  def tryDeleteFile(file: Path): Boolean =
    tryDeleteFiles(file :: Nil)

  def tryDeleteFiles(files: Iterable[Path]): Boolean =
    var allFilesDeleted = true
    for file <- files do
      try
        logger.whenDebugEnabled:
          if exists(file) then logger.debug(s"Delete file '$file'")
        deleteIfExists(file)
      catch case NonFatal(t) =>
        allFilesDeleted = false
        logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}")
    allFilesDeleted
