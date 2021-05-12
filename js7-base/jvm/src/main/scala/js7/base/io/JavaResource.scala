package js7.base.io

import cats.effect.SyncIO
import java.io.{File, InputStream}
import java.net.{URI, URL}
import java.nio.file.{CopyOption, DirectoryNotEmptyException, FileAlreadyExistsException, Files, Path}
import java.util.Objects.requireNonNull
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.io.JavaResource._
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import scala.language.implicitConversions

/**
 * @author Joacim Zschimmer
 */
final case class JavaResource(classLoader: ClassLoader, path: String)
{
  requireNonNull(classLoader)
  require(!(path startsWith "/"), s"JavaResource must not start with a slash: $path")

  private lazy val checkedUrl: Checked[URL] =
    classLoader.getResource(path) match {
      case null => Left(Problem(s"Unknown JavaResource '$path'"))
      case url =>
        logger.trace(s"Using JavaResource $url")
        Right(url)
    }

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
  def copyToFiles(resourceNames: Iterable[String], directory: Path, copyOptions: CopyOption*): Seq[Path] = {
    val resourcePathAndDllFiles = for (name <- resourceNames) yield (this / name, directory resolve name)
    for ((resourcePath, file) <- resourcePathAndDllFiles) {
      resourcePath.copyToFile(file, copyOptions: _*)   // After an exception here, already created files are left !!!
    }
    resourcePathAndDllFiles.toVector.map(_._2)
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
    autoClosing(openStream()) { in =>
      Files.copy(in, file, copyOptions: _*)
    }
    file
  }

  def contentBytes: Array[Byte] =
    readAs[ByteArray].unsafeArray

  def readAs[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq]): ByteSeq =
    autoClosing(openStream())(ByteSeq.fromInputStreamUnlimited)

  def asUTF8String = readAs[ByteArray].utf8String

  def simpleName = new File(path).getName

  def isValid = checkedUrl.isRight

  val asResource: cats.effect.Resource[SyncIO, InputStream] =
    cats.effect.Resource.fromAutoCloseable(SyncIO { openStream() })

  def openStream(): InputStream = url.openStream()

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def uri: URI = url.toURI

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def url: URL = checkedUrl.orThrow

  def /(tail: String) = copy(path = s"${path stripSuffix "/"}/$tail")

  override def toString = path
}

object JavaResource {
  private val logger = Logger[this.type]

  def apply(classLoader: ClassLoader, path: String): JavaResource =
    new JavaResource(classLoader, path)

  def apply(path: String): JavaResource = JavaResource(classOf[JavaResource].getClassLoader, path)

  ///** Uses the caller's class ClassLoader. */
  //def apply(path: String): JavaResource = macro apply_macro
  //
  //def apply_macro(c: scala.reflect.macros.blackbox.Context)(path: c.Expr[String]): c.Tree = {
  //  import c.universe._
  //  q"_root_.js7.base.io.JavaResource(this.getClass.getClassLoader, $path)"
  //}

  implicit def asResource(o: JavaResource): cats.effect.Resource[SyncIO, InputStream] =
    o.asResource
}
