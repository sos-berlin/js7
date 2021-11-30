package js7.data.event

trait SignedItemContainer extends ItemContainer
{
  //def keyToSignedItem: MapView[SignableItemKey, Signed[SignableItem]]
  //
  //private final def keyToSigned[I <: SignableItem](I: SignableItem.Companion[I])
  //: MapView[I.Key, Signed[I]] =
  //  keyToSignedItem
  //    .filter { case (_, v) => I.cls.isAssignableFrom(v.getClass) }
  //    .asInstanceOf[MapView[I.Key, Signed[I]]]
}
