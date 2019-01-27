package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.RichGenericCompanion
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.filebased.TypedPaths.fileToTypedPath
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import java.nio.file.Files.newDirectoryStream
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
object TypedPathDirectoryWalker {

  private val NestingLimit = 100

  @deprecated
  def typedFiles(directory: Path, companions: Iterable[TypedPath.AnyCompanion]): Seq[Checked[TypedFile]] =
    unorderedTypedFiles(directory, companions) sortBy (_._1) map (_._2)  // Sorted for deterministic test results

  @deprecated
  private def unorderedTypedFiles(directory: Path, companions: Iterable[TypedPath.AnyCompanion]) =
    Vector.build[(Path, Checked[TypedFile])] { builder ⇒
      deepForEachPathAndAttributes(directory, nestingLimit = NestingLimit) { (file, attr) ⇒
        if (!attr.isDirectory) {
          if (!file.startsWith(directory)) {
            builder += file → Problem(s"Path '$file' does not start with '$directory'")
          } else {
            builder += file → fileToTypedFile(directory, file, companions)
          }
        }
      }
    }

  def fileToTypedFile(baseDirectory: Path, path: Path, companions: Iterable[TypedPath.AnyCompanion]): Checked[TypedFile] =
    fileToTypedPath(companions, baseDirectory, path) .map(o ⇒ TypedFile(path, o._1, o._2))

  def checkUniqueness(typedFiles: Seq[TypedFile]): Checked[typedFiles.type] = {
    val duplicateFiles: Iterable[Path] =
      typedFiles groupBy (_.path) filter (_._2.lengthCompare(2) >= 0) flatMap (_._2 map (_.file))
    if (duplicateFiles.isEmpty)
      Valid(typedFiles)
    else
      Invalid(Problem(s"Duplicate configuration files: ${duplicateFiles.toVector.sorted mkString ", "}"))
  }

  /**
    * @param nestingLimit to avoid StackOverflowException and symbolic recursion
    */
  private def deepForEachPathAndAttributes(directory: Path, nestingLimit: Int)(callback: (Path, BasicFileAttributes) ⇒ Unit): Unit =
    autoClosing(newDirectoryStream(directory)) { stream ⇒
      for (path ← stream.iterator.asScala) {
        val attrs = Files.readAttributes(path, classOf[BasicFileAttributes]) // IOException in case of invalid symbolic link
        callback(path, attrs)
        if (attrs.isDirectory) {
          if (nestingLimit <= 0) throw new RuntimeException(s"Directory hierarchy is nested too deeply: $directory")
          deepForEachPathAndAttributes(path, nestingLimit - 1)(callback)
        }
      }
    }

  final case class TypedFile(file: Path, path: TypedPath, sourceType: SourceType)
}
