package js7.data_for_java.common

import js7.data_for_java.common.MoreJavaConverters.MapViewHasAsJava
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.{MapView, mutable}
import scala.jdk.CollectionConverters.*

final class JMapViewWrapperTest extends AnyFreeSpec
{
  "test" in {
    assert(MapView.empty.asJava.isEmpty)

    val mapView = MapView(1 -> "EINS", 2 -> "ZWEI")
    val jMapView: java.util.Map[Int, String] = mapView.asJava

    assert(!jMapView.isEmpty)
    assert(jMapView.size == 2)
    assert(jMapView.get(1) == "EINS")
    assert(jMapView.get(2) == "ZWEI")
    assert(jMapView.get(3) == null)
    new mutable.HashSet()
    assert(jMapView.keySet().asScala == Set(1, 2))
    assert(jMapView.values().asScala.toSeq == Seq("EINS", "ZWEI"))

    // TODO Requires Java 9:
    //assert(jMapView.entrySet.asScala == Set(
    //  java.util.Map.entry(1, "EINS"),
    //  java.util.Map.entry(2, "ZWEI")))
  }
}
