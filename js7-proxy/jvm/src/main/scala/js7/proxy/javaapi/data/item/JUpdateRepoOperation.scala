package js7.proxy.javaapi.data.item

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
  /** `signedString` contains the JSON-serialized `InventoryItem` including path and `VersionId`,
    * signed with a signature. */
  def addOrReplace(signedString: SignedString) =
    new JUpdateRepoOperation(UpdateRepoOperation.AddOrReplace(signedString))

  def delete(path: ItemPath) =
    new JUpdateRepoOperation(UpdateRepoOperation.Delete(path))
}
