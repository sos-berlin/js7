package com.sos.scheduler.engine.common.utils

import com.google.common.base.Charsets._
import com.google.common.io.ByteStreams.toByteArray
import com.google.common.io.{ByteStreams, Resources}
import com.google.common.io.Resources.getResource
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.utils.JavaResource._
import java.io.File
import java.net.{URI, URL}
import java.nio.file.{CopyOption, DirectoryNotEmptyException, FileAlreadyExistsException, Files, Path}
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final case class JavaResource(path: String) {
  require(!(path startsWith "/"), s"JavaResource must not start with a slash: $path")

  def requireExistence() = {
    url
    this
  }

  /**
    * Copies the resource files denoted by `resourceNames` name by name to `directory`.
    *
    * @return The created file paths
    * @throws FileAlreadyExistsException
    * if the target file exists but cannot be replaced because
    * the `REPLACE_EXISTING` option is not specified <i>(optional specific exception)</i>
    * @throws DirectoryNotEmptyException
    * the `REPLACE_EXISTING` option is specified but the file cannot be replaced because
    * it is a non-empty directory <i>(optional specific exception)</i>
    */
  def copyToFiles(resourceNames: Iterable[String], directory: Path, copyOptions: CopyOption*): immutable.Seq[Path] = {
    val resourcePathAndDllFiles = for (name ← resourceNames) yield (this / name, directory resolve name)
    for ((resourcePath, file) ← resourcePathAndDllFiles) {
      resourcePath.copyToFile(file, copyOptions: _*)   // After an exception here, already created files are left !!!
    }
    resourcePathAndDllFiles.toVector map { _._2 }
  }

  /**
    * Copies the resource to a new file with java.nio.file.Files.copy.
    *
    * @throws FileAlreadyExistsException
    * if the target file exists but cannot be replaced because
    * the `REPLACE_EXISTING` option is not specified <i>(optional specific exception)</i>
    * @throws DirectoryNotEmptyException
    * the `REPLACE_EXISTING` option is specified but the file cannot be replaced because
    * it is a non-empty directory <i>(optional specific exception)</i>
    */
  def copyToFile(file: Path, copyOptions: CopyOption*): Path = {
    autoClosing(url.openStream()) { in ⇒
      Files.copy(in, file, copyOptions: _*)
    }
    file
  }

  def contentBytes: Array[Byte] = autoClosing(url.openStream())(toByteArray)

  def asUTF8String = Resources.toString(url, UTF_8)

  def simpleName = new File(path).getName

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  lazy val url: URL =
    getResource(path) sideEffect { u ⇒ logger.trace(s"Using Resource $u") }

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def uri: URI = url.toURI

  def /(tail: String) = JavaResource(s"${path stripSuffix "/"}/$tail")

  override def toString = path

}

object JavaResource {
  private val logger = Logger(getClass)

  def apply(o: Package) = new JavaResource(o.getName.replace('.', '/'))
}
