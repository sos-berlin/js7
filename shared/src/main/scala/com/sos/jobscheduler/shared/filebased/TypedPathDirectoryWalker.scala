package com.sos.jobscheduler.shared.filebased

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.shared.filebased.TypedPaths.xmlFileToTypedPath
import java.nio.file
import java.nio.file.Files.newDirectoryStream
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
object TypedPathDirectoryWalker {

  private val NestingLimit = 100

  //def typedFiles[P <: TypedPath: TypedPath.Companion](directory: Path): immutable.Seq[(Path, P)] = {
  //  val seq = Vector.newBuilder[(Path, P)]
  //  forEachTypedFile(directory, Set(implicitly[TypedPath.Companion[P]])) {
  //    case (path: Path, typedPath: TypedPath) ⇒ seq += path → typedPath.asInstanceOf[P]
  //  }
  //  seq.result
  //}

  def forEachTypedFile(directory: Path, types: Set[TypedPath.AnyCompanion])(callback: (Path, TypedPath) ⇒ Unit): Unit =
    deepForEachPathAndAttributes(directory, nestingLimit = NestingLimit) { (path, attr) ⇒
      if (!attr.isDirectory) {
        for (t ← types find { t ⇒ matchesFile(t, path) }) {
          callback(path, xmlFileToTypedPath(path, stripDirectory = directory)(t))
        }
      }
    }

  private[filebased] def matchesFile[P <: TypedPath](companion: TypedPath.Companion[P], path: file.Path): Boolean =
    path.toString endsWith companion.xmlFilenameExtension

  //def typedFileIterator(directory: Path, types: Set[TypedPath.AnyCompanion]): AutoCloseable with Iterator[(Path, TypedPath)] =
  //  new AbstractIterator[(Path, TypedPath)] with AutoCloseable {
  //    def close() = ?
  //
  //    def hasNext = ?
  //
  //    def next() = ?
  //  }
  //
  ///**
  //  * @param nestingLimit to avoid StackOverflowException and symbolic recursion
  //  */
  //private def deepPathAndAttributeIterator(directory: Path, nestingLimit: Int): AutoCloseable with Iterator[(Path, BasicFileAttributes)] =
  //  new AbstractIterator[(Path, BasicFileAttributes)] with AutoCloseable {
  //    private val stack = mutable.Stack[DirectoryStream[Path]]()
  //    private var iterator = newIterator(directory)
  //    private def newIterator(directory: Path) = {
  //      val stream = newDirectoryStream(directory)
  //      stack.push(newDirectoryStream(directory))
  //      for (path ← stream.iterator) {
  //        val attrs = Files.readAttributes(path, classOf[BasicFileAttributes]) // IOException in case of invalid symbolic link
  //        callback(path, attrs)
  //        if (attrs.isDirectory) {
  //          stack.push(stream)
  //          if (nestingLimit <= 0) throw new RuntimeException(s"Directory hierarchy is nested to deeply: $directory")
  //          deepForEachPathAndAttributes(path, nestingLimit - 1)(callback)
  //        }
  //      }
  //    }
  //
  //    def close() = {
  //      while (stack.nonEmpty) {
  //        stack.pop().close()
  //      }
  //    }
  //
  //    def hasNext = {
  //      iterator.hasNext
  //    }
  //
  //    def next() = ?
  //  }
  //  autoClosing() { stream ⇒
  //    for (path ← stream.iterator) {
  //      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes]) // IOException in case of invalid symbolic link
  //      callback(path, attrs)
  //      if (attrs.isDirectory) {
  //        if (nestingLimit <= 0) throw new RuntimeException(s"Directory hierarchy is nested to deeply: $directory")
  //        deepForEachPathAndAttributes(path, nestingLimit - 1)(callback)
  //      }
  //    }
  //  }


  /**
    * @param nestingLimit to avoid StackOverflowException and symbolic recursion
    */
  private def deepForEachPathAndAttributes(directory: Path, nestingLimit: Int)(callback: (Path, BasicFileAttributes) ⇒ Unit): Unit =
    autoClosing(newDirectoryStream(directory)) { stream ⇒
      for (path ← stream.iterator.asScala) {
        val attrs = Files.readAttributes(path, classOf[BasicFileAttributes]) // IOException in case of invalid symbolic link
        callback(path, attrs)
        if (attrs.isDirectory) {
          if (nestingLimit <= 0) throw new RuntimeException(s"Directory hierarchy is nested to deeply: $directory")
          deepForEachPathAndAttributes(path, nestingLimit - 1)(callback)
        }
      }
    }
}
