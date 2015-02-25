package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.base.IsString

trait AbsolutePath extends IsString {

  final def name: String = string.substring(string.lastIndexOf('/') + 1)

  final def parent: String =
    string lastIndexOf '/' match {
      case 0 if string == "/" ⇒ throw new IllegalStateException("Root path has not parent folder")
      case 0 ⇒ "/"
      case n ⇒ string.substring(0, n)
    }

  final def withTrailingSlash: String = if (string endsWith "/") string else s"$string/"

  final def withoutStartingSlash: String = string stripPrefix "/"

  /** Has to be called in every implementing constructor. */
  protected def requireIsAbsolute(): Unit = require(string startsWith "/", s"Absolute path expected: $toString")
}


object AbsolutePath {

  /**
   * @param path ist absolut oder relativ zur Wurzel.
   */
  @Deprecated
  def of(path: String): AbsolutePath = UntypedPath(makeAbsolute(path))

  /**
   * @param path A string starting with "./" is rejected
   */
  def makeAbsolute(path: String): String =
    if (path startsWith "/") path
    else {
      require(!(path startsWith "./"), s"Relative path is not possible here: $path")
      s"/$path"
    }

  /**
   * A `path` starting with "./" is prefixed with `defaultBase` (while removing "./").
   * Other paths are assumed to be absolute already, even if the starting slash is missing.
   */
  def makeCompatibleAbsolute(defaultFolder: String, path: String) = {
    val defaultPath = UntypedPath(defaultFolder)
    if (path startsWith "./") s"${defaultPath.withTrailingSlash}${path.substring(2)}"
    else if (path startsWith "/") path
    else s"/$path"
  }

  @Deprecated
  def of(parentPath: AbsolutePath, subpath: String): AbsolutePath = {
    val a = new StringBuilder
    a.append(stripTrailingSlash(parentPath.string))
    a.append('/')
    a.append(subpath)
    of(a.toString())
  }

  private def stripTrailingSlash(a: String): String =
    if (a endsWith "/") a.substring(0, a.length - 1) else a

  private case class UntypedPath(string: String) extends AbsolutePath {
    requireIsAbsolute()
  }
}
