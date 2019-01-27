package com.sos.jobscheduler.common.files

import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import java.nio.file.Files.newDirectoryStream
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{DirectoryStream, Files, Path}
import java.util.Comparator

/**
  * @author Joacim Zschimmer
  */
object DirectoryReader
{
  private val NestingLimit = 100

  def entries(directory: Path, filter: Path ⇒ Boolean = _ ⇒ true): Vector[Entry] = {
    val array = unorderedEntryArray(directory, filter)
    java.util.Arrays.parallelSort(array, Entry.comparator)  // Uses Java's ThreadPool.common
    array.toVector
  }

  private def unorderedEntryArray(directory: Path, filter: Path ⇒ Boolean): Array[Entry] =
    Array.build { entries ⇒
      deepForEachPathAndAttributes(directory, p ⇒ filter(p), nestingLimit = NestingLimit) { entry ⇒
        entries += entry
      }
    }

  /**
    * @param nestingLimit to avoid StackOverflowException and symbolic recursion
    */
  private def deepForEachPathAndAttributes(rootDirectory: Path, filter: DirectoryStream.Filter[Path], nestingLimit: Int)
    (callback: Entry ⇒ Unit): Unit
  = {
    def nest(dir: Path, nestingLimit: Int): Unit = {
      autoClosing(newDirectoryStream(dir, filter))(_ forEach { path ⇒
        val attr = Files.readAttributes(path, classOf[BasicFileAttributes])
        if (attr.isDirectory) {
          if (nestingLimit <= 0) throw new RuntimeException(s"Directory hierarchy is nested too deeply: $dir")
          nest(path, nestingLimit - 1)
        } else {
          if (!path.startsWith(dir)) sys.error(s"File path '$path' does not start with '$dir' ?")
          callback(Entry(path, attr))
        }
      })
    }
    nest(rootDirectory, nestingLimit)
  }

  def fileIsTouched(a: BasicFileAttributes, b: BasicFileAttributes) =
    a.isDirectory != b.isDirectory ||
    a.isRegularFile != b.isRegularFile ||
    a.isSymbolicLink != b.isSymbolicLink ||
    a.isOther != b.isOther ||
    a.creationTime != b.creationTime ||
    a.lastModifiedTime != b.lastModifiedTime ||
    a.size != b.size ||
    a.fileKey != b.fileKey

  final case class Entry(path: Path, attributes: BasicFileAttributes) {
    override def equals(other: Any) =
      other match {
        case o: Entry ⇒ path == o.path && !fileIsTouched(attributes, o.attributes)
        case _ ⇒ false
      }

    def isTouched(o: BasicFileAttributes) = fileIsTouched(attributes, o)
  }
  object Entry {
    val comparator: Comparator[Entry] = (a, b) ⇒ a.path compareTo b.path
  }
}
