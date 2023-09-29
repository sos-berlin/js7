package js7.common.files

import java.nio.file.Files.newDirectoryStream
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{DirectoryStream, Files, Path}
import java.util.Comparator
import js7.base.utils.AutoClosing.autoClosing
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object DirectoryReader:
  private val NestingLimit = 100

  def files(directory: Path, filter: Path => Boolean = _ => true): Seq[Path] =
    entries(directory, filter).map(_.file)

  def entries(directory: Path, filter: Path => Boolean = _ => true): Seq[Entry] =
    val array = unorderedEntryArray(directory, filter)
    java.util.Arrays.parallelSort(array, Entry.comparator)  // Uses Java's ThreadPool.common
    array.toVector

  private def unorderedEntryArray(directory: Path, filter: Path => Boolean): Array[Entry] =
    val builder = mutable.ArrayBuilder.make[Entry]
    deepForEachPathAndAttributes(directory, p => filter(p), nestingLimit = NestingLimit) { entry =>
      builder += entry
    }
    builder.result()

  /**
    * @param nestingLimit to avoid StackOverflowException and symbolic recursion
    */
  private def deepForEachPathAndAttributes(rootDirectory: Path, filter: DirectoryStream.Filter[Path], nestingLimit: Int)
    (callback: Entry => Unit): Unit
  =
    def nest(dir: Path, nestingLimit: Int): Unit =
      autoClosing(newDirectoryStream(dir, filter))(_ forEach { path =>
        val attr = Files.readAttributes(path, classOf[BasicFileAttributes])
        if attr.isDirectory then {
          if nestingLimit <= 0 then throw new RuntimeException(s"Directory hierarchy is nested too deeply: $dir")
          nest(path, nestingLimit - 1)
        } else {
          if !path.startsWith(dir) then sys.error(s"File path '$path' does not start with '$dir' ?")
          callback(Entry(path, attr))
        }
      })
    nest(rootDirectory, nestingLimit)

  def fileIsTouched(a: BasicFileAttributes, b: BasicFileAttributes) =
    a.isDirectory != b.isDirectory ||
    a.isRegularFile != b.isRegularFile ||
    a.isSymbolicLink != b.isSymbolicLink ||
    a.isOther != b.isOther ||
    a.creationTime != b.creationTime ||
    a.lastModifiedTime != b.lastModifiedTime ||
    a.size != b.size ||
    a.fileKey != b.fileKey

  final case class Entry(file: Path, attributes: BasicFileAttributes):
    override def equals(other: Any) =
      other match
        case o: Entry => file == o.file && !fileIsTouched(attributes, o.attributes)
        case _ => false

    def isTouched(o: BasicFileAttributes) = fileIsTouched(attributes, o)
  object Entry:
    val comparator: Comparator[Entry] = (a, b) => a.file compareTo b.file
