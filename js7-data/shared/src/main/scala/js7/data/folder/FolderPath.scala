package js7.data.folder

import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.data.item.{ItemPath, SourceType}

final case class FolderPath private(string: String) extends ItemPath {
  import FolderPath._

  def companion = FolderPath

  def subfolder(name: String): FolderPath = {
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(
      if (isRoot)
        string
      else
        s"${string stripSuffix "/"}/$name")
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
    isRoot ||
      (path.string startsWith withTrailingSlash) ||
      PartialFunction.cond(path) {
        case path: FolderPath => this == path
      }

  def isRoot = string.isEmpty

  override def toFile(t: SourceType) =
    throw new NotImplementedError("FolderPath.toFile")  // In Scala.js, don't use java.nio.file.Paths
}

object FolderPath extends ItemPath.Companion[FolderPath]
{
  val Root = new FolderPath("")
  val sourceTypeToFilenameExtension = Map.empty

  protected def unchecked(string: String) = new FolderPath(string)

  override def checked(string: String): Checked[FolderPath] =
    if (string == Root.string)
      Right(Root)
    else
      super.checked(string)

  def parentOf(path: ItemPath): FolderPath =
    path.string lastIndexOf '/' match {
      case -1 =>
        if (path == FolderPath.Root) throw new IllegalStateException("Root path has no parent folder")
        FolderPath.Root
      case n => FolderPath(path.string.substring(0, n))
    }

  /**
   * An absolute `path` starting with "/" is used as given.
   * A relative `path` not starting with "/" is used relative to `defaultFolder`.
   */
  private def absoluteString(folder: FolderPath, path: String): String = {
    val glue = if (folder.string.isEmpty || folder.string.endsWith("/")) "" else "/"
    s"${folder.string}$glue$path"
  }
}
