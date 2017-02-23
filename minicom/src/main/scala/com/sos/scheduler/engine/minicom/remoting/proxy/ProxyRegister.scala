package com.sos.scheduler.engine.minicom.remoting.proxy

import com.google.common.collect.HashBiMap
import com.sos.scheduler.engine.common.scalautil.AssignableFrom.assignableFrom
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister._
import com.sos.scheduler.engine.minicom.types.HRESULT.E_POINTER
import com.sos.scheduler.engine.minicom.types.{COMException, IUnknown}
import javax.annotation.Nullable
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
private[remoting] final class ProxyRegister {
  private val proxyIdToIUnknown = HashBiMap.create[ProxyId, IUnknown]()
  private val _iUnknownToProxyId = proxyIdToIUnknown.inverse
  private val proxyIdGenerator = ProxyId.newGenerator()

  def registerProxy(proxy: ProxyIDispatch): Unit = add(proxy.id, proxy)

  def iUnknownToProxyId(@Nullable iUnknown: IUnknown): (ProxyId, Boolean) = {
    if (iUnknown == null)
      (ProxyId.Null, false)
    else
      synchronized {
        _iUnknownToProxyId.get(iUnknown) match {
          case null ⇒
            val proxyId = proxyIdGenerator.next()
            add(proxyId, iUnknown)
            (proxyId, true)
          case o ⇒ (o, false)
        }
      }
  }

  private def add(proxyId: ProxyId, iUnknown: IUnknown): Unit =
    synchronized {
      if (proxyIdToIUnknown containsKey proxyId) throw new DuplicateKeyException(s"$proxyId already registered")
      if (_iUnknownToProxyId containsKey iUnknown) throw new DuplicateKeyException(s"IUnknown '$iUnknown' already registered")
      proxyIdToIUnknown.put(proxyId, iUnknown)
    }

  def release(proxyId: ProxyId): Unit =
    synchronized { proxyIdToIUnknown.remove(proxyId) }
    match {
      case o: AutoCloseable ⇒
        try o.close()
        catch { case NonFatal(t) ⇒ logger.error(s"Suppressed: $t", t) }
      case _ ⇒
    }

  def iUnknown(proxyId: ProxyId): IUnknown = {
    if (proxyId == ProxyId.Null) throw new COMException(E_POINTER)
    synchronized { proxyIdToIUnknown(proxyId) }
  }

  override def toString = s"${getClass.getSimpleName}($size proxies)"

  /**
   * Returns all iUnknowns implementing the erased type A.
   */
  def iUnknowns[A <: IUnknown: ClassTag]: immutable.Iterable[A] =
    synchronized {
      (proxyIdToIUnknown.valuesIterator collect assignableFrom[A]).toVector
    }

  def size: Int = proxyIdToIUnknown.size
}

private[remoting] object ProxyRegister {
  private val logger = Logger(getClass)

  class DuplicateKeyException(override val getMessage: String) extends RuntimeException
}
