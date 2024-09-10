package js7.base.utils

import scala.collection.MapView

trait StandardMapView[K, +V] extends MapView[K, V]:

  override final def iterator: Iterator[(K, V)] =
    keysIterator.flatMap(k => get(k).map(k -> _))

  override final def keysIterator: Iterator[K] =
    keySet.iterator
