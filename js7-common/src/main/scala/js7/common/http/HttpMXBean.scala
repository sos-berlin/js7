package js7.common.http

import cats.effect.Resource
import cats.effect.kernel.Sync
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*

sealed trait HttpMXBean:
  this: HttpMXBean.Bean.type =>

  def getClientRequestTotal: Long = clientRequestTotal.get
  def getClientRequestActiveCount: Int = clientRequestActiveCount.get
  def getClientSentByteTotal: Long = clientSentByteTotal.get
  def getClientReceivedByteTotal: Long = clientReceivedByteTotal.get

  def getServerRequestTotal: Long = serverRequestTotal.get
  def getServerRequestActiveCount: Int = serverRequestActiveCount.get
  def getServerSentByteTotal: Long = serverSentByteTotal.get
  def getServerReceivedByteTotal: Long = serverReceivedByteTotal.get


object HttpMXBean:
  object Bean extends HttpMXBean:
    val clientRequestTotal: Atomic[Long] = Atomic(0L)
    val clientRequestActiveCount: Atomic[Int] = Atomic(0)
    val clientSentByteTotal: Atomic[Long] = Atomic(0)
    val clientReceivedByteTotal: Atomic[Long] = Atomic(0)

    val serverRequestTotal: Atomic[Long] = Atomic(0L)
    val serverRequestActiveCount: Atomic[Int] = Atomic(0)
    val serverSentByteTotal: Atomic[Long] = Atomic(0)
    val serverReceivedByteTotal: Atomic[Long] = Atomic(0)


object HttpMXBeanUtils:
  import HttpMXBean.Bean

  def clientRequestResource[F[_]: Sync as F]: Resource[F, Unit] =
    Resource.make(
      acquire = F.delay:
        Bean.clientRequestTotal += 1
        Bean.clientRequestActiveCount += 1)(
    release = n => F.delay:
      Bean.clientRequestActiveCount -= 1)
