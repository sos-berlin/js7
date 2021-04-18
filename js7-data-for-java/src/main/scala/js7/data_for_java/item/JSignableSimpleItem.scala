package  js7.data_for_java.item

import javax.annotation.Nonnull
import js7.data.item.{SignableSimpleItem, SignableSimpleItemId}

trait JSignableSimpleItem extends JSignableItem
{
  protected type AsScala = SignableSimpleItem

  def asScala: SignableSimpleItem

  @Nonnull
  def id: SignableSimpleItemId
}
