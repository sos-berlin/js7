package js7.base.io

import cats.effect.kernel.Sync
import cats.effect.{Resource, SyncIO}
import io.circe.Decoder
import java.io.{File, IOException, InputStream, InputStreamReader, OutputStream, Reader}
import java.net.{URI, URL}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{CopyOption, Files, Path}
import java.util.Objects.requireNonNull
import java.util.Properties
import js7.base.data.{ByteArray, ByteSequence, Writable}
import js7.base.io.JavaResource.UnknownJavaResourceProblem
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import scala.collection.immutable.Map.Map1
import scala.language.implicitConversions

/**
 * @author Joacim Zschimmer
 */
final case class JavaResource(classLoader: ClassLoader, path: String):

  requireNonNull(classLoader)
  require(!path.startsWith("/"), s"JavaResource must not start with a slash: $path")

  private lazy val checkedUrl: Checked[URL] =
    classLoader.getResource(path) match
      case null => Left(UnknownJavaResourceProblem(path))
      case url => Right(url)

  def exists: Boolean =
    !checkedUrl.left.exists(_.isInstanceOf[UnknownJavaResourceProblem])

  def requireExistence(): JavaResource =
    url
    this

  /**
    * Copies the resource files denoted by `resourceNames` name by name to `directory`.
    *
    * @return The created file paths
    * @throws java.nio.file.FileAlreadyExistsException
    * if the target file exists but cannot be replaced because
    * the `REPLACE_EXISTING` option is not specified <i>(optional specific exception)</i>
    * @throws java.nio.file.DirectoryNotEmptyException
    * the `REPLACE_EXISTING` option is specified but the file cannot be replaced because
    * it is a non-empty directory <i>(optional specific exception)</i>
    */
  def copyToFiles(resourceNames: Iterable[String], directory: Path, copyOptions: CopyOption*)
  : Seq[Path] =
    val resourcePathAndDllFiles = for name <- resourceNames yield
      (this / name, directory.resolve(name))
    for (resourcePath, file) <- resourcePathAndDllFiles do
      resourcePath.copyToFile(file, copyOptions*)   // After an exception here, already created files are left !!!
    resourcePathAndDllFiles.toVector.map(_._2)

  /**
    * Copies the resource to a new file with java.nio.file.Files.copy.
    *
    * @throws java.nio.file.FileAlreadyExistsException
    * if the target file exists but cannot be replaced because
    * the `REPLACE_EXISTING` option is not specified <i>(optional specific exception)</i>
    * @throws java.nio.file.DirectoryNotEmptyException
    * the `REPLACE_EXISTING` option is specified but the file cannot be replaced because
    * it is a non-empty directory <i>(optional specific exception)</i>
    */
  def copyToFile(file: Path, copyOptions: CopyOption*): Path =
    autoClosing(openStream()): in =>
      Files.copy(in, file, copyOptions*)
    file

  def writeToStream(out: OutputStream): Unit =
    autoClosing(openStream())(_.transferTo(out))

  def contentBytes: Array[Byte] =
    readAs[ByteArray].unsafeArray

  def jsonAs[A: Decoder]: Checked[A] =
    readAs[ByteArray].parseJsonAs[A]

  def readAs[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq]): ByteSeq =
    autoClosing(openStream())(ByteSeq.fromInputStreamUnlimited)

  def asUTF8String: String =
    readAs[ByteArray].utf8String

  def toProperties: Properties =
    val props = new Properties
    val in = openStream()
    try
      props.load(in)
    finally
      in.close()
    props

  def simpleName: String =
    new File(path).getName

  def isValid: Boolean = checkedUrl.isRight

  def inputStream[F[_]: Sync as F]: Resource[F, InputStream] =
    Resource.fromAutoCloseable(F.delay(openStream()))

  def reader[F[_]: Sync as F]: Resource[F, Reader] =
    inputStream[F].map: in =>
      new InputStreamReader(in, UTF_8)

  def openStream(): InputStream =
    classLoader.getResourceAsStream(path) match
      case null => throw new IOException(s"Resource not found: $path")
      case o => o

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def uri: URI = url.toURI

  /**
   * @throws RuntimeException, if the resource does not exists.
   */
  def url: URL = checkedUrl.orThrow

  def /(tail: String): JavaResource =
    copy(path = s"${path stripSuffix "/"}/$tail")

  override def toString: String = path


object JavaResource:

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

  def asResource(o: JavaResource): Resource[SyncIO, InputStream] =
    o.inputStream

  implicit val writable: Writable[JavaResource] = _.writeToStream(_)

  final case class UnknownJavaResourceProblem(path: String) extends Problem.Coded:
    def arguments = Map1("path", path)
