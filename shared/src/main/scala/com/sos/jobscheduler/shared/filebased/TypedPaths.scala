package com.sos.jobscheduler.shared.filebased

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.data.filebased.TypedPath
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object TypedPaths {

  def jsonFileToTypedPath[P <: TypedPath: TypedPath.Companion](path: Path, stripDirectory: Path): P =
    fileToTypedPath[P](_.jsonFilenameExtension, path, stripDirectory)

  def textFileToTypedPath[P <: TypedPath: TypedPath.Companion](path: Path, stripDirectory: Path): P =
    fileToTypedPath[P](_.txtFilenameExtension, path, stripDirectory)

  def xmlFileToTypedPath[P <: TypedPath: TypedPath.Companion](path: Path, stripDirectory: Path): P =
    fileToTypedPath[P](_.xmlFilenameExtension, path, stripDirectory)

  private def fileToTypedPath[P <: TypedPath: TypedPath.Companion]
  (extension: TypedPath.Companion[P] ⇒ String, path: Path, stripDirectory: Path): P
  = {
    val normalizedPath = path.toString.replaceAllLiterally(path.getFileSystem.getSeparator, "/")
    val normalizedDir = (if (stripDirectory.toString endsWith "/") s"$stripDirectory" else s"$stripDirectory/")
      .replace(path.getFileSystem.getSeparator, "/")
    if (!(normalizedPath startsWith normalizedDir)) throw new IllegalArgumentException("Path does not match directory")
    val p = normalizedPath stripPrefix normalizedDir
    assert(s"$normalizedDir$p" == normalizedPath)
    relativeFilePathToTypedPath[P](extension, p)
  }

  @VisibleForTesting
  private[filebased] def relativeFilePathToTypedPath[P <: TypedPath: TypedPath.Companion]
  (extension: TypedPath.Companion[P] ⇒ String, path: String): P
  = {
    val normalized = path.replaceAllLiterally("\\", "/")
    if (normalized startsWith "/") throw new IllegalArgumentException(s"Relative path expected: $path")
    val companion = implicitly[TypedPath.Companion[P]]
    val ext = extension(companion)
    if (!(normalized endsWith ext)) throw new IllegalArgumentException(s"Filename extension '$ext' expected: $path")
    val p = normalized.stripSuffix(ext)
    assert(s"$p$ext" == normalized)
    companion.apply(s"/$p")
  }
}
