package js7.common.http

import cats.effect.Resource
import cats.effect.kernel.Sync
import java.util.concurrent.atomic.LongAdder
import js7.base.utils.Atomic.extensions.*

sealed trait HttpMXBean:
  this: HttpMXBean.Bean.type =>

  def getClientRequestTotal: Long = clientRequestTotal.longValue
  def getClientRequestActiveCount: Int = clientRequestActiveCount.intValue
  def getClientSentByteTotal: Long = clientSentByteTotal.longValue
  def getClientReceivedByteTotal: Long = clientReceivedByteTotal.longValue

  def getServerRequestTotal: Long = serverRequestTotal.longValue
  def getServerRequestActiveCount: Int = serverRequestActiveCount.intValue
  def getServerSentByteTotal: Long = serverSentByteTotal.longValue
  def getServerReceivedByteTotal: Long = serverReceivedByteTotal.longValue


object HttpMXBean:
  object Bean extends HttpMXBean:
    val clientRequestTotal: LongAdder = new LongAdder
    val clientRequestActiveCount: LongAdder = new LongAdder
    val clientSentByteTotal: LongAdder = new LongAdder
    val clientReceivedByteTotal: LongAdder = new LongAdder

    val serverRequestTotal: LongAdder = new LongAdder
    val serverRequestActiveCount: LongAdder = new LongAdder
    val serverSentByteTotal: LongAdder = new LongAdder
    val serverReceivedByteTotal: LongAdder = new LongAdder


object HttpMXBeanUtils:
  import HttpMXBean.Bean

  def clientRequestResource[F[_]: Sync as F]: Resource[F, Unit] =
    Resource.make(
      acquire = F.delay:
        Bean.clientRequestTotal += 1
        Bean.clientRequestActiveCount += 1)(
    release = n => F.delay:
      Bean.clientRequestActiveCount -= 1)
