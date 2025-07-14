package js7.base.system

import cats.effect.{IO, Resource, ResourceIO, Sync}
import cats.syntax.flatMap.*
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.utils.Tests.{isScalaTest, isTest}
import scala.util.NotGiven

object MBeanUtils:
  private val DomainName = "js7"
  private val beanServer = ManagementFactory.getPlatformMBeanServer

  def registerMBean[B](name: String, newBean: => B)(using NotGiven[B <:< IO[?]]): ResourceIO[B] =
    registerMBean(name = name)(IO(newBean))

  def registerMBean[F[_]: Sync as F, B](name: String)(newBean: F[B]): Resource[F, B] =
    registerMBean(toMBeanName(name))(newBean)

  def registerMBean[F[_]: Sync as F, B](objectName: ObjectName)(newBean: F[B]): Resource[F, B] =
    Resource
      .make(
        acquire =
          newBean.flatMap: bean =>
            F.delay:
              try
                beanServer.registerMBean(bean, objectName)
                bean -> true
              catch case e: javax.management.InstanceAlreadyExistsException
                if isTest | isScalaTest =>
                // Give up, don't register and don't unregister !!!
                // Requires a system test without isTest!
                bean -> false
              )(
        release = (bean, isRegistered) =>
          F.whenA(isRegistered):
            F.delay:
              beanServer.unregisterMBean(objectName))
      .map(_._1)

  def toMBeanName(name: String): ObjectName =
    new ObjectName(s"$DomainName:name=$name")
