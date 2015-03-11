package com.sos.scheduler.engine.minicom.remoting.proxy

import com.google.common.collect.HashBiMap
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.Invocable
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister._
import com.sos.scheduler.engine.minicom.types.COMException
import com.sos.scheduler.engine.minicom.types.HRESULT.E_POINTER
import org.scalactic.Requirements._
import scala.collection.JavaConversions._
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
private[remoting] final class ProxyRegister {
  private val proxyIdToInvocable = HashBiMap.create[ProxyId, Invocable]()
  private val invocableToProxyId = proxyIdToInvocable.inverse
  private val proxyIdGenerator = ProxyId.newGenerator()

  def registerProxy(proxy: ProxyIDispatch): Unit = add(proxy.id, proxy)

  def invocableToProxyId(invocable: Invocable): (ProxyId, Boolean) = {
    requireNonNull(invocable)
    synchronized {
      invocableToProxyId.get(invocable) match {
        case null ⇒
          val proxyId = proxyIdGenerator.next()
          add(proxyId, invocable)
          (proxyId, true)
        case o ⇒ (o, false)
      }
    }
  }

  private def add(proxyId: ProxyId, invocable: Invocable): Unit =
    synchronized {
      if (proxyIdToInvocable containsKey proxyId) throw new DuplicateKeyException(s"$proxyId already registered")
      if (invocableToProxyId containsKey invocable) throw new DuplicateKeyException(s"IUnknown '$invocable' already registered")
      proxyIdToInvocable.put(proxyId, invocable)
    }

  def release(proxyId: ProxyId): Unit =
    synchronized {
      proxyIdToInvocable.remove(proxyId) match {
        case o: AutoCloseable ⇒
          try o.close()
          catch { case NonFatal(t) ⇒ logger.error(s"Suppressed: $t", t) }
        case _ ⇒
      }
    }

  def invocable(proxyId: ProxyId): Invocable = {
    if (proxyId == ProxyId.Null) throw new COMException(E_POINTER)
    synchronized { proxyIdToInvocable(proxyId) }
  }

  override def toString = s"${getClass.getSimpleName}($size proxies)"

  def size = proxyIdToInvocable.size
}

private[remoting] object ProxyRegister {
  private val logger = Logger(getClass)

  class DuplicateKeyException(override val getMessage: String) extends RuntimeException
}
