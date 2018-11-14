package com.sos.jobscheduler.common.utils

import cats.data.Validated.{Invalid, Valid}
import com.google.common.base.Charsets._
import com.google.common.io.ByteStreams.toByteArray
import com.google.common.io.Resources
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import java.io.File
import java.net.{URI, URL}
import java.nio.file.{CopyOption, DirectoryNotEmptyException, FileAlreadyExistsException, Files, Path}
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final case class JavaResource(path: String)
{
  require(!(path startsWith "/"), s"JavaResource must not start with a slash: $path")

  private lazy val checkedUrl = JavaResource.url(path)

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

  def isValid = checkedUrl.isValid

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def uri: URI = url.toURI

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def url: URL = checkedUrl.orThrow

  def /(tail: String) = JavaResource(s"${path stripSuffix "/"}/$tail")

  override def toString = path
}

object JavaResource {
  private val logger = Logger(getClass)

  def apply(o: Package) = new JavaResource(o.getName.replace('.', '/'))

  private def url(resourceName: String): Checked[URL] = {
    val classLoader = Option(Thread.currentThread.getContextClassLoader) getOrElse classOf[JavaResource].getClassLoader
    classLoader.getResource(resourceName) match {
      case null ⇒ Invalid(Problem(s"Unknown JavaResource '$resourceName'"))
      case url ⇒
        logger.trace(s"Using JavaResource $url")
        Valid(url)
    }
  }
}
