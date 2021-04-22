package  js7.data_for_java.item

import javax.annotation.Nonnull
import js7.base.crypt.SignedString
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, DeleteSimple, DeleteVersioned}
import js7.data.item.{ItemOperation, ItemPath, SimpleItemPath, VersionId}
import js7.data_for_java.common.JavaWrapper

final case class JUpdateItemOperation(asScala: ItemOperation)
extends JavaWrapper
{
  protected type AsScala = ItemOperation
}

object JUpdateItemOperation
{
  @Nonnull
  def addOrChangeSimple(@Nonnull item: JUnsignedSimpleItem) =
    new JUpdateItemOperation(ItemOperation.AddOrChangeSimple(item.asScala))

  /** Deletes a non-versioned item. */
  @Nonnull
  def deleteSimple(@Nonnull path: SimpleItemPath): JUpdateItemOperation =
    new JUpdateItemOperation(
      DeleteSimple(path))

  /** Required exactly once if any of addReplace(SignedString) or deleteItem(ItemPath) is used. */
  def addVersion(versionId: VersionId) =
    new JUpdateItemOperation(
      AddVersion(versionId))

  /** Please use addOrChangedSigned instead! */
  @Deprecated
  @Nonnull
  def addOrChangeVersioned(@Nonnull signedString: SignedString): JUpdateItemOperation =
    addOrChangeSigned(signedString)

  /** `signedString` contains the JSON-serialized `VersionedItem` including path and `VersionId`,
    * signed with a signature.
    * Adds the versioned item to the versioned specified with `addVersion`. */
  @Nonnull
  def addOrChangeSigned(@Nonnull signedString: SignedString): JUpdateItemOperation =
    new JUpdateItemOperation(
      AddOrChangeSigned(signedString))

  /** BITTE VERWENDE deleteVersioned! */
  @Deprecated
  @Nonnull
  def deleteItem(@Nonnull path: ItemPath): JUpdateItemOperation =
    deleteVersioned(path)

  /** Delete the given path in the new version,
    * Requires `addVersion`. */
  @Nonnull
  def deleteVersioned(@Nonnull path: ItemPath): JUpdateItemOperation =
    new JUpdateItemOperation(
      DeleteVersioned(path))
}
