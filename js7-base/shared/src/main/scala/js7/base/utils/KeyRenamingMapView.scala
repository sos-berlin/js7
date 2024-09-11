package js7.base.utils

import scala.collection.MapView

final class KeyRenamingMapView[K, +V](internToExternKey: Iterable[(K, K)])(mapView: MapView[K, V])
extends MapView[K, V]:

  private lazy val (toExtern, toIntern) =
    (internToExternKey.toMap, internToExternKey.map(_.swap).toMap)

  def get(k: K): Option[V] =
    toIntern.get(k).flatMap(mapView.get)

  def iterator: Iterator[(K, V)] =
    mapView.iterator.map: (k, v) =>
      toExtern(k) -> v

  override def keysIterator: Iterator[K] =
    mapView.keySet.iterator.map(toExtern)

  override def keySet: collection.Set[K] =
    mapView.keySet.map(toExtern)
