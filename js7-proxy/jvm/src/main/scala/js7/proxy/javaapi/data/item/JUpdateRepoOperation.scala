package js7.proxy.javaapi.data.item

import javax.annotation.Nonnull
import js7.base.crypt.SignedString
import js7.data.item.{ItemOperation, ItemPath}
import js7.proxy.javaapi.data.common.JavaWrapper

@Deprecated // Use JUpdateItemOperation
final case class JUpdateRepoOperation(asScala: ItemOperation.UpdateRepoOperation)
extends JavaWrapper
{
  protected type AsScala = ItemOperation.UpdateRepoOperation
}

object JUpdateRepoOperation
{
  /** `signedString` contains the JSON-serialized `VersionedItem` including path and `VersionId`,
    * signed with a signature. */
  @Deprecated
  @Nonnull
  def addOrReplace(@Nonnull signedString: SignedString) =
    new JUpdateRepoOperation(ItemOperation.VersionedAddOrReplace(signedString))

  @Nonnull
  @Deprecated
  def delete(@Nonnull path: ItemPath) =
    new JUpdateRepoOperation(ItemOperation.VersionedDelete(path))
}
