package js7.base.system

import com.sun.management.OperatingSystemMXBean
import java.lang.management.ManagementFactory.getPlatformMXBean
import java.lang.management.PlatformManagedObject
import js7.base.utils.Lazy
import scala.collection.MapView

// EXPERIMENTAL, placed in test directory !!!
private object MXBeanMapView:

  private val supportedPlatformManagedObjects: Seq[Class[_ <: PlatformManagedObject]] = Seq(
    classOf[com.sun.management.OperatingSystemMXBean],
    classOf[java.lang.management.MemoryMXBean],
    classOf[java.lang.management.OperatingSystemMXBean],
    classOf[java.lang.management.RuntimeMXBean],
    classOf[java.lang.management.ThreadMXBean])

  private val nameToLazyBeanView: Map[String, Lazy[BeanMapView[PlatformManagedObject]]] =
    supportedPlatformManagedObjects
      .map(_.asInstanceOf[Class[PlatformManagedObject]])
      .map: cls =>
        cls.getName -> Lazy(BeanMapView(cls, getPlatformMXBean(cls)))
      .toMap

  val mxBeanMapView: MapView[String, MapView[String, Any]] =
    new MapView[String, MapView[String, Any]]:
      def get(key: String) =
        nameToLazyBeanView.get(key)
          .map: mapViewLazy =>
            mapViewLazy.value

      def iterator =
        nameToLazyBeanView
          .iterator
          .map: (k, v) =>
            k -> v.value

      override def keysIterator: Iterator[String] =
        nameToLazyBeanView.keysIterator

      override def keySet: Set[String] =
        nameToLazyBeanView.keySet
