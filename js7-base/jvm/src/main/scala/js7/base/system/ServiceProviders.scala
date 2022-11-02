package js7.base.system

import java.util.ServiceLoader
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

object ServiceProviders
{
  private val logger = Logger[this.type]

  def findServices[A](callback: (=> String, Option[A]) => Unit = defaultCallback[A])
    (implicit A: ClassTag[A]): Seq[A] = {
    val interface = A.runtimeClass.asInstanceOf[Class[A]]
    val serviceLoader = ServiceLoader.load(
      interface,
      getClass.getClassLoader/*required for testing with sbt, sometimes*/)

    val iterator = serviceLoader.iterator.asScala
    if (iterator.isEmpty)
      callback(
        s"No ${interface.simpleScalaName}",
        None)
    else
      for (service <- iterator/*loads services lazily*/) {
        val cls = service.getClass
        val where = Option(cls.getProtectionDomain.getCodeSource)
          .fold("")(o => s" in ${o.getLocation}")
        callback(
          s"Found service provider $service · ${cls.scalaName}$where",
          Some(service))
      }

    serviceLoader.asScala.toVector
  }

  private def defaultCallback[A](logLine: => String, service: Option[A]): Unit =
    logger.debug(logLine)
}
