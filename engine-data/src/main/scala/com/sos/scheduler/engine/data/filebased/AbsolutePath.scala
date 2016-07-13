package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.data.folder.FolderPath
import org.jetbrains.annotations.TestOnly

trait AbsolutePath extends IsString {

  final def name: String = string.substring(string.lastIndexOf('/') + 1)

  final def parent: FolderPath =
    string lastIndexOf '/' match {
      case 0 if string == "/" ⇒ throw new IllegalStateException("Root path has not parent folder")
      case 0 ⇒ FolderPath.Root
      case n ⇒ FolderPath(string.substring(0, n))
    }

  final def withTrailingSlash: String = if (string endsWith "/") string else s"$string/"

  final def withoutStartingSlash: String = string stripPrefix "/"

  /** Has to be called in every implementing constructor. */
  protected def requireIsAbsolute(): Unit = require(string startsWith "/", s"Absolute path expected: $toString")
}


object AbsolutePath {

  /**
   * An absolute `path` starting with "/" is used as given.
   * A relative `path` not starting with "/" is used relative to `defaultFolder`.
   */
  private def absoluteString(defaultFolder: FolderPath, path: String): String = {
    if (path startsWith "/") path
    else s"${defaultFolder.withTrailingSlash}${path stripPrefix "./"}"
  }

  /**
   * @param path ist absolut oder relativ zur Wurzel.
   */
  @Deprecated
  def of(path: String): AbsolutePath = UntypedPath(absoluteString(path))

  /**
   * Interprets a path as absolute.
   *
   * @param path A string starting with "./" is rejected
   */
  private def absoluteString(path: String): String =
    if (path startsWith "/") path
    else {
      require(!(path startsWith "./"), s"Relative path is not possible here: $path")
      s"/$path"
    }

  /** NOT USED (delete this code at some time).
   * A `path` starting with "./" is prefixed with `defaultFolder` (while removing "./").
   * Other paths are assumed to be absolute already, even if the starting slash is missing.
   */
  @TestOnly
  //@deprecated("New policy for paths without starting slash: it should be considered be relative", "1.9")
  private[filebased] def makeCompatibleAbsolute(defaultFolder: String, path: String): String = {
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

  trait Companion[A <: AbsolutePath] extends IsString.Companion[A] {
    def apply(o: String): A

    /**
     * Interprets a path as absolute.
     *
     * @param path A string starting with "./" is rejected
     */
    final def makeAbsolute(path: String) = apply(absoluteString(path))

    /**
     * An absolute `path` starting with "/" is used as given.
     * A relative `path` not starting with "/" is used relative to `defaultFolder`.
     */
    final def makeAbsolute(defaultFolder: FolderPath, path: String) = apply(absoluteString(defaultFolder, path))
  }

  private case class UntypedPath(string: String) extends AbsolutePath {
    requireIsAbsolute()
  }
}
