package js7.data_for_java.item

import javax.annotation.Nonnull
import js7.data.item.{SignableSimpleItem, SignableSimpleItemPath}

trait JSignableSimpleItem extends JSignableItem
{
  protected type AsScala = SignableSimpleItem

  def asScala: SignableSimpleItem

  @Nonnull
  def path: SignableSimpleItemPath
}
