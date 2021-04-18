package  js7.data_for_java.item

import javax.annotation.Nonnull
import js7.data.item.{SimpleItemId, UnsignedSimpleItem}

trait JUnsignedSimpleItem
{
  def asScala: UnsignedSimpleItem

  @Nonnull
  def id: SimpleItemId
}
