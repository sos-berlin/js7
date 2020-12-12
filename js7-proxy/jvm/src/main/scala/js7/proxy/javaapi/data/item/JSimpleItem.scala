package js7.proxy.javaapi.data.item

import javax.annotation.Nonnull
import js7.data.item.{SimpleItem, SimpleItemId}

trait JSimpleItem
{
  def asScala: SimpleItem

  @Nonnull
  def id: SimpleItemId
}
