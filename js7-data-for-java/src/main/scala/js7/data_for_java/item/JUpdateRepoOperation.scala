package  js7.data_for_java.item

import js7.data.item.ItemOperation
import js7.data_for_java.common.JavaWrapper

@Deprecated // Use JUpdateItemOperation
final case class JUpdateRepoOperation(asScala: ItemOperation.VersionedOperation)
extends JavaWrapper
{
  protected type AsScala = ItemOperation.VersionedOperation
}
