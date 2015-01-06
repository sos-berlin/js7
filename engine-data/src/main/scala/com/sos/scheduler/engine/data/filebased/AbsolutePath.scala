package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.base.IsString

trait AbsolutePath extends IsString {

  final def name: String = {
    val s = string
    s.substring(s.lastIndexOf('/') + 1)
  }

  final def withTrailingSlash: String = {
    val r = string
    if (r endsWith "/") r else r + "/"
  }

  final def withoutStartingSlash: String = {
    val s = string
    assert(s startsWith "/")
    s.substring(1)
  }

  final def requireIsEmptyOrAbsolute(): Unit = {
    if (!isEmpty)
      requireIsAbsolute()
  }

  final def requireIsAbsolute(): Unit = {
    require(isAbsolute, s"Absolute path expected: $toString")
  }

  final def isAbsolute =
    string startsWith "/"
}


object AbsolutePath {

  /** @param path ist absolut oder relativ zur Wurzel. */
  @Deprecated
  def of(path: String): AbsolutePath =
    new AbsolutePath { def string = makeAbsolute(path) }

  def makeAbsolute(path: String): String =
    if (path.startsWith("/")) path else "/" + path

  @Deprecated
  def of(parentPath: AbsolutePath, subpath: String): AbsolutePath = {
    val a = new StringBuilder
    a.append(stripTrailingSlash(parentPath.string))
    a.append('/')
    a.append(subpath)
    of(a.toString())
  }

  private def stripTrailingSlash(a: String): String =
    if (a.endsWith("/")) a.substring(0, a.length - 1) else a
}

