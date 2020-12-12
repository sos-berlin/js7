package js7.proxy.javaapi.data.item

import javax.annotation.Nonnull
import js7.base.crypt.SignedString
import js7.data.item.{ItemPath, UpdateRepoOperation}
import js7.proxy.javaapi.data.common.JavaWrapper

final case class JUpdateRepoOperation(asScala: UpdateRepoOperation.ItemOperation)
extends JavaWrapper
{
  protected type AsScala = UpdateRepoOperation.ItemOperation
}

object JUpdateRepoOperation
{
  /** `signedString` contains the JSON-serialized `VersionedItem` including path and `VersionId`,
    * signed with a signature. */
  @Nonnull
  def addOrReplace(@Nonnull signedString: SignedString) =
    new JUpdateRepoOperation(UpdateRepoOperation.AddOrReplace(signedString))

  @Nonnull
  def delete(@Nonnull path: ItemPath) =
    new JUpdateRepoOperation(UpdateRepoOperation.Delete(path))
}
