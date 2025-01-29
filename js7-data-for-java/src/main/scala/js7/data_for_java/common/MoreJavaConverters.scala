package js7.data_for_java.common

import scala.collection.MapView

object MoreJavaConverters:

  extension [K, V](mapView: MapView[K, V])
    def asJava: java.util.Map[K, V] =
      mapView match
        //case w: JMapViewWrapper[K @unchecked, V @unchecked] => w.underlying
        case _ => new JMapViewWrapper(mapView)
