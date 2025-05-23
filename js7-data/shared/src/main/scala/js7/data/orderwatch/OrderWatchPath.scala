package js7.data.orderwatch

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class OrderWatchPath(string: String)
extends UnsignedSimpleItemPath, InventoryItemPath.AttachableToAgent:

  protected type Self = OrderWatchPath

  val companion: OrderWatchPath.type = OrderWatchPath

  inline def /(externalOrderName: ExternalOrderName): ExternalOrderKey =
    ExternalOrderKey(this, externalOrderName)


object OrderWatchPath extends UnsignedSimpleItemPath.Companion[OrderWatchPath]:
  type Item = OrderWatch

  @javaApi
  def of(id: String): OrderWatchPath =
    mayThrow(id)

  override protected def unchecked(string: String) =
    new OrderWatchPath(string)
