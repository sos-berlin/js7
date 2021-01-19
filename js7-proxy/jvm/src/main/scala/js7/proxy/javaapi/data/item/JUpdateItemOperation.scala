package js7.proxy.javaapi.data.item

import javax.annotation.Nonnull
import js7.base.crypt.SignedString
import js7.data.item.ItemOperation.{AddVersion, SimpleDelete, VersionedAddOrChange, VersionedDelete}
import js7.data.item.{ItemOperation, ItemPath, SimpleItemId, VersionId}
import js7.proxy.javaapi.data.common.JavaWrapper

final case class JUpdateItemOperation(asScala: ItemOperation)
extends JavaWrapper
{
  protected type AsScala = ItemOperation
}

object JUpdateItemOperation
{
  /** BITTE VERWENDE addOrChangeSimple!
    *
    * Adds or replaces a non-versioned item. */
  @Deprecated
  @Nonnull
  def addOrChange(@Nonnull item: JSimpleItem) =
    addOrChangeSimple(item)

  @Nonnull
  def addOrChangeSimple(@Nonnull item: JSimpleItem) =
    new JUpdateItemOperation(ItemOperation.SimpleAddOrChange(item.asScala))

  /** BITTE VERWENDE deleteSimple!
    *
    * Deletes a non-versioned item. */
  @Deprecated
  @Nonnull
  def deleteItem(@Nonnull itemId: SimpleItemId) =
    deleteSimple(itemId)

  /** Deletes a non-versioned item. */
  @Nonnull
  def deleteSimple(@Nonnull itemId: SimpleItemId): JUpdateItemOperation =
    new JUpdateItemOperation(
      SimpleDelete(itemId))

  /** Required exactly once if any of addReplace(SignedString) or deleteItem(ItemPath) is used. */
  def addVersion(versionId: VersionId) =
    new JUpdateItemOperation(
      AddVersion(versionId))

  /** BITTE VERWENDE addOrChangeVersioned!
    *
    * `signedString` contains the JSON-serialized `VersionedItem` including path and `VersionId`,
    * signed with a signature.
    * Adds the versioned item to the versioned specified with `addVersion`. */
  @Deprecated
  @Nonnull
  def addOrChange(@Nonnull signedString: SignedString): JUpdateItemOperation =
    addOrChangeVersioned(signedString)

  /** `signedString` contains the JSON-serialized `VersionedItem` including path and `VersionId`,
    * signed with a signature.
    * Adds the versioned item to the versioned specified with `addVersion`. */
  @Nonnull
  def addOrChangeVersioned(@Nonnull signedString: SignedString): JUpdateItemOperation =
    new JUpdateItemOperation(
      VersionedAddOrChange(signedString))

  /** BITTE VERWENDE deleteVersioned!
    *
    * Delete the given path in the new version,
    * Requires `addVersion`. */
  @Nonnull
  def deleteItem(@Nonnull path: ItemPath): JUpdateItemOperation =
    deleteVersioned(path)

  /** Delete the given path in the new version,
    * Requires `addVersion`. */
  @Nonnull
  def deleteVersioned(@Nonnull path: ItemPath): JUpdateItemOperation =
    new JUpdateItemOperation(
      VersionedDelete(path))
}
