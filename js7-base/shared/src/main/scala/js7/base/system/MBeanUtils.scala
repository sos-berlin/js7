package js7.base.system

import cats.effect.{IO, Resource, ResourceIO, Sync, SyncIO}
import cats.syntax.flatMap.*
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.log.Logger
import js7.base.utils.Base64UUID
import js7.base.utils.Tests.{isScalaTest, isTest}
import scala.util.NotGiven
import scala.util.control.NonFatal

object MBeanUtils:
  private val logger = Logger[this.type]
  private val DomainName = "js7"
  private val beanServer = ManagementFactory.getPlatformMBeanServer

  /** Bean is considered static and will not be deregistered under testing.
    * <p>This is to allow concurrently running tests. */
  def registerStaticMBean[B](name: String, newBean: B)(using NotGiven[B <:< IO[?]]): ResourceIO[B] =
    registerMBean[IO, B](toMBeanName(name), isStatic = true)(IO.pure(newBean))

  def registerMBean[F[_]: Sync as F]: RegisterMBean[F] =
    new RegisterMBean[F]

  final class RegisterMBean[F[_]: Sync as F]:
    def apply[B](name: String, newBean: => B)
      (using NotGiven[B <:< IO[?]], NotGiven[B <:< SyncIO[?]])
    : Resource[F, B] =
      registerMBean[F, B](name)(F.delay(newBean))

  def registerMBean[F[_]: Sync, B](name: String)(newBean: F[B]): Resource[F, B] =
    registerMBean[F, B](toMBeanName(name))(newBean)

  def registerMBean[F[_] : Sync as F, B](objectName: ObjectName, isStatic: Boolean = false)
    (newBean: F[B])
  : Resource[F, B] =
    Resource
      .make(
        acquire =
          newBean.flatMap: bean =>
            F.delay:
              logger.trace(s"registerMBean($objectName)")
              if isStatic then
                try
                  beanServer.registerMBean(bean, objectName)
                  bean -> None // Never deregister
                catch case e: javax.management.InstanceAlreadyExistsException =>
                  bean -> None // Never deregister
              else
                try
                  beanServer.registerMBean(bean, objectName)
                  bean -> Some(objectName)
                catch case e: javax.management.InstanceAlreadyExistsException
                  if isTest | isScalaTest =>
                  val uuid = Base64UUID.randomString()
                  val testObjectName = new ObjectName(objectName.toString + s",test=$uuid")
                  logger.trace(s"registerMBean($objectName): $e • trying $testObjectName")
                  try
                    beanServer.registerMBean(bean, testObjectName)
                    bean -> Some(testObjectName)
                  catch case NonFatal(t) =>
                    logger.error(s"registerMBean($objectName): $t")
                    throw t)(
        release =
          case (bean, None) => F.unit
          case (bean, Some(objectName)) =>
            F.delay:
              beanServer.unregisterMBean(objectName))
      .map(_._1)

  def toMBeanName(name: String): ObjectName =
    new ObjectName(s"$DomainName:name=$name")
