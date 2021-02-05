package  js7.data_for_java.item

import javax.annotation.Nonnull
import js7.data.item.{SimpleItem, SimpleItemId}

trait JSimpleItem
{
  def asScala: SimpleItem

  @Nonnull
  def id: SimpleItemId
}
