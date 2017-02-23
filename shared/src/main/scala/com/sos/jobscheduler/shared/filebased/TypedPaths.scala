package com.sos.jobscheduler.shared.filebased

import com.sos.jobscheduler.data.filebased.TypedPath
import java.nio.file.{Path, Paths}

/**
  * @author Joacim Zschimmer
  */
object TypedPaths {

  def fileToTypedPath[P <: TypedPath: TypedPath.Companion](path: Path, stripDirectory: Path): P = {
    val normalizedPath = path.toString.replaceAllLiterally(path.getFileSystem.getSeparator, "/")
    val normalizedDir = (if (stripDirectory.toString endsWith "/") stripDirectory else Paths.get(s"$stripDirectory/"))
      .toString.replace(path.getFileSystem.getSeparator, "/")
    if (!(normalizedPath.toString startsWith normalizedDir.toString)) throw new IllegalArgumentException("Path does not match directory")
    val p = normalizedPath.toString
      .stripPrefix(normalizedDir.toString + "/")
    assert(s"$normalizedDir/$p" == normalizedPath)
    relativeFilePathToTypedPath[P](p)
  }

  def relativeFilePathToTypedPath[P <: TypedPath: TypedPath.Companion](path: String): P = {
    val normalized = path.replaceAllLiterally("\\", "/")
    if (normalized startsWith "/") throw new IllegalArgumentException(s"Relative path expected: $path")
    val companion = implicitly[TypedPath.Companion[P]]
    val ext = companion.filenameExtension
    if (!(normalized.toString endsWith ext)) throw new IllegalArgumentException(s"Filename extension '$ext' expected: $path")
    val p = normalized.toString
      .stripSuffix(ext)
    assert(s"$p$ext" == normalized)
    companion.apply(s"/$p")
  }
}
