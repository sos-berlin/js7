package js7.proxy.javaapi.data.item

import javax.annotation.Nonnull
import js7.base.crypt.SignedString
import js7.data.item.ItemOperation.AddVersion
import js7.data.item.{ItemOperation, ItemPath, SimpleItem, SimpleItemId, VersionId}
import js7.proxy.javaapi.data.common.JavaWrapper

final case class JUpdateItemOperation(asScala: ItemOperation)
extends JavaWrapper
{
  protected type AsScala = ItemOperation
}

object JUpdateItemOperation
{
  /** Adds or replaces a non-versioned item. */
  @Nonnull
  def addOrChange(@Nonnull item: JSimpleItem) =
    new JUpdateItemOperation(ItemOperation.SimpleAddOrChange(item.asScala))

  /** Deletes a non-versioned item. */
  @Nonnull
  def deleteItem(@Nonnull itemId: SimpleItemId) =
    new JUpdateItemOperation(ItemOperation.SimpleDelete(itemId))

  /** Required exactly once if any of addReplace(SignedString) or deleteItem(ItemPath) is used. */
  def addVersion(versionId: VersionId) =
    new JUpdateItemOperation(AddVersion(versionId))

  /** `signedString` contains the JSON-serialized `VersionedItem` including path and `VersionId`,
    * signed with a signature.
    * Adds the versioned item to the versioned specified with `addVersion`. */
  @Nonnull
  def addOrChange(@Nonnull signedString: SignedString) =
    new JUpdateItemOperation(ItemOperation.VersionedAddOrChange(signedString))

  /** Delete the given path in the new version,
    * Requires `addVersion`. */
  @Nonnull
  def deleteItem(@Nonnull path: ItemPath) =
    new JUpdateItemOperation(ItemOperation.VersionedDelete(path))
}
