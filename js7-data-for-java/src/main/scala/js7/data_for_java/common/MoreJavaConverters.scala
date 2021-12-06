package js7.data_for_java.common

import scala.collection.MapView

object MoreJavaConverters
{
  implicit final class MapViewHasAsJava[K, V](private val mapView: MapView[K, V]) extends AnyVal
  {
    def asJava: java.util.Map[K, V] =
      mapView match {
        case null => null
        //case w: JMapViewWrapper[K @unchecked, V @unchecked] => w.underlying
        case _ => new JMapViewWrapper(mapView)
      }
  }
}
