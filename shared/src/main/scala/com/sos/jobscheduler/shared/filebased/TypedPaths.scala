package com.sos.jobscheduler.shared.filebased

import com.google.common.annotations.VisibleForTesting
import com.sos.jobscheduler.data.filebased.TypedPath
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object TypedPaths {

  def xmlFileToTypedPath[P <: TypedPath: TypedPath.Companion](path: Path, stripDirectory: Path): P = {
    val normalizedPath = path.toString.replaceAllLiterally(path.getFileSystem.getSeparator, "/")
    val normalizedDir = (if (stripDirectory.toString endsWith "/") s"$stripDirectory" else s"$stripDirectory/")
      .replace(path.getFileSystem.getSeparator, "/")
    if (!(normalizedPath startsWith normalizedDir)) throw new IllegalArgumentException("Path does not match directory")
    val p = normalizedPath stripPrefix normalizedDir
    assert(s"$normalizedDir$p" == normalizedPath)
    relativeXmlFilePathToTypedPath[P](p)
  }

  @VisibleForTesting
  private[filebased] def relativeXmlFilePathToTypedPath[P <: TypedPath: TypedPath.Companion](path: String): P = {
    val normalized = path.replaceAllLiterally("\\", "/")
    if (normalized startsWith "/") throw new IllegalArgumentException(s"Relative path expected: $path")
    val companion = implicitly[TypedPath.Companion[P]]
    val ext = companion.xmlFilenameExtension
    if (!(normalized endsWith ext)) throw new IllegalArgumentException(s"Filename extension '$ext' expected: $path")
    val p = normalized.stripSuffix(ext)
    assert(s"$p$ext" == normalized)
    companion.apply(s"/$p")
  }
}
