package js7.base.utils

import scala.collection.MapView

object MoreJavaConverters:

  extension [K, V](mapView: MapView[K, V])
    def asJava: java.util.Map[K, V] =
      mapView match
        //case w: MapViewJavaWrapper[K @unchecked, V @unchecked] => w.underlying
        case _ => new MapViewJavaWrapper(mapView)
