package js7.data.orderwatch

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemPath

final case class OrderWatchPath(string: String) extends UnsignedSimpleItemPath
{
  protected type Self = OrderWatchPath

  val companion = OrderWatchPath
}

object OrderWatchPath extends UnsignedSimpleItemPath.Companion[OrderWatchPath]
{
  def itemTypeName = "OrderWatch"

  @javaApi
  def of(id: String) = apply(id)

  override protected def unchecked(string: String) =
    new OrderWatchPath(string)
}
