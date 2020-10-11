package js7.data.folder

import java.util.UUID.randomUUID
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.data.item.{SourceType, ItemPath}

final case class FolderPath private(string: String) extends ItemPath {
  import FolderPath._

  def companion = FolderPath

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(s"${string stripSuffix "/"}/$name")
  }

  /**
    * Resolves the given path agains this FolderPath.
    * <ul>
    *   <li>An absolute `path` starting with "/" is used as given.
    *   <li>A relative `path` (not starting with "/") is used relative to this `FolderPath`.
    * </ul>
   */
  def resolve[P <: ItemPath: ItemPath.Companion](path: String): P =
    checkedResolve[P](path).orThrow

  /**
    * Resolves the given path agains this FolderPath.
    * <ul>
    *   <li>An absolute `path` starting with "/" is used as given.
    *   <li>A relative `path` (not starting with "/") is used relative to this `FolderPath`.
    * </ul>
   */
  def checkedResolve[P <: ItemPath: ItemPath.Companion](path: String): Checked[P] =
    implicitly[ItemPath.Companion[P]].checked(absoluteString(this, path))

  def isParentOf(path: ItemPath): Boolean =
    path != FolderPath.Root && this == parentOf(path)

  def isAncestorOf(path: ItemPath): Boolean =
    (path.string startsWith withTrailingSlash) ||
      PartialFunction.cond(path) {
        case path: FolderPath => this == path
      }

  override def toFile(t: SourceType) = throw new NotImplementedError("FolderPath.toFile")  // In Scala.js, don't use java.nio.file.Paths
}

object FolderPath extends ItemPath.Companion[FolderPath]
{
  val Root = FolderPath("/")
  val Internal = FolderPath(ItemPath.InternalPrefix stripSuffix "/")
  val sourceTypeToFilenameExtension = Map.empty

  override def isSingleSlashAllowed = true

  protected def unchecked(string: String) = new FolderPath(string)

  def fromTrailingSlash(string: String) = {
    require(string endsWith "/", "Trailing slash required for FolderPath")
    FolderPath(if (string == "/") string else string stripSuffix "/")
  }

  def parentOf(path: ItemPath): FolderPath =
    path.string lastIndexOf '/' match {
      case 0 if path.string == "/" => throw new IllegalStateException("Root path has no parent folder")
      case 0 => FolderPath.Root
      case -1 => FolderPath.Root // In case of ProcessClass.Default (the empty string)
      case n => FolderPath(path.string.substring(0, n))
    }

  /**
   * An absolute `path` starting with "/" is used as given.
   * A relative `path` not starting with "/" is used relative to `defaultFolder`.
   */
  private def absoluteString(defaultFolder: FolderPath, path: String): String =
    if (ItemPath.isAbsolute(path))
      path
    else
      s"${defaultFolder.withTrailingSlash}${path stripPrefix "./"}"

  def random[P <: ItemPath: ItemPath.Companion]: P =
    Internal resolve[P] randomUUID.toString

  def anonymous[P <: ItemPath: ItemPath.Companion]: P =
    Internal resolve[P] "anonymous"
}
