package js7.data.folder

import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.data.item.{SourceType, VersionedItemPath}

final case class FolderPath private(string: String) extends VersionedItemPath:
  import FolderPath.*

  def companion = FolderPath

  def subfolder(name: String): FolderPath =
    require(!name.contains('/'), "Name must not contain a slash '/'")
    FolderPath(
      if isRoot then
        string
      else
        s"$string/$name")

  /**
    * Appends the given path to this FolderPath and returns a `P`.
   */
  def resolve[P <: VersionedItemPath: VersionedItemPath.Companion](path: String): P =
    checkedResolve[P](path).orThrow

  /**
    * Appends the given path to this FolderPath and returns a `P`.
   */
  def checkedResolve[P <: VersionedItemPath: VersionedItemPath.Companion](path: String): Checked[P] =
    implicitly[VersionedItemPath.Companion[P]].checked(absoluteString(this, path))

  def isParentOf(path: VersionedItemPath): Boolean =
    path != FolderPath.Root && this == parentOf(path)

  def isAncestorOf(path: VersionedItemPath): Boolean =
    isRoot ||
      (path.string startsWith withTrailingSlash) ||
      PartialFunction.cond(path):
        case path: FolderPath => this == path

  def isRoot = string.isEmpty

  override def toFile(t: SourceType) =
    throw new NotImplementedError("FolderPath.toFile")  // In Scala.js, don't use java.nio.file.Paths

object FolderPath extends VersionedItemPath.Companion[FolderPath]:
  val Root = new FolderPath("")
  override val sourceTypeToFilenameExtension = Map.empty

  protected def unchecked(string: String) = new FolderPath(string)

  override def checked(string: String): Checked[FolderPath] =
    if string == Root.string then
      Right(Root)
    else
      super.checked(string)

  def parentOf(path: VersionedItemPath): FolderPath =
    path.string lastIndexOf '/' match
      case -1 =>
        if path == FolderPath.Root then throw new IllegalStateException("Root path has no parent folder")
        FolderPath.Root
      case n => FolderPath(path.string.substring(0, n))

  /**
   * An absolute `path` starting with "/" is used as given.
   * A relative `path` not starting with "/" is used relative to `defaultFolder`.
   */
  private def absoluteString(folder: FolderPath, path: String): String =
    val glue = if folder.string.isEmpty then "" else "/"
    s"${folder.string}$glue$path"
