package com.sos.scheduler.engine.minicom.remoting.proxy

import com.google.inject.Guice
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.minicom.idispatch.{IDispatch, Invocable}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister.DuplicateKeyException
import com.sos.scheduler.engine.minicom.types.COMException
import com.sos.scheduler.engine.minicom.types.HRESULT._
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProxyRegisterTest extends FreeSpec {

  private val proxyRegister = Guice.createInjector().instance[ProxyRegister]
  private val externalProxyId = ProxyId(0x123456789abcdefL)

  "External ProxyId" in {
    val proxy = newProxy(externalProxyId)
    proxyRegister.registerProxy(proxy)
    proxy.id shouldEqual externalProxyId
    proxyRegister.invocableToProxyId(proxy) shouldEqual (externalProxyId, false)
    proxyRegister.invocable(externalProxyId) shouldEqual proxy
    proxyRegister.size shouldEqual 1
    intercept[DuplicateKeyException] { proxyRegister.registerProxy(newProxy(externalProxyId)) }
  }

  "Own IDispatch" in {
    proxyRegister.size shouldEqual 1
    val iDispatch = mock[Invocable]
    val (proxyId, true) = proxyRegister.invocableToProxyId(iDispatch)
    proxyId.index shouldEqual 1
    proxyRegister.invocableToProxyId(iDispatch) shouldEqual (proxyId, false)
    proxyRegister.invocable(proxyId) shouldEqual iDispatch
    intercept[DuplicateKeyException] { proxyRegister.registerProxy(newProxy(proxyId)) }

    proxyRegister.size shouldEqual 2
    val (otherProxyId, true) = proxyRegister.invocableToProxyId(mock[Invocable])
    otherProxyId.index shouldEqual 2
    proxyRegister.size shouldEqual 3
  }

  "null is rejected" in {
    intercept[COMException] { proxyRegister.invocable(ProxyId.Null) } .hResult shouldEqual E_POINTER
    intercept[NullPointerException] { proxyRegister.invocableToProxyId(null) }
  }

  "removeProxy" in {
    proxyRegister.size shouldEqual 3
    proxyRegister.release(externalProxyId)
    proxyRegister.size shouldEqual 2
    proxyRegister.release(externalProxyId)
    proxyRegister.size shouldEqual 2
  }

  "remoteProxy closes AutoCloseable" in {
    trait A extends IDispatch with AutoCloseable
    val a = mock[A]
    when (a.close()) thenThrow new Exception("SHOULD BE IGNORED, ONLY LOGGED")
    val (proxyId, true) = proxyRegister.invocableToProxyId(a)
    proxyRegister.release(proxyId)
    verify(a).close()
  }

  private def newProxy(proxyId: ProxyId, name: String = "") = new SimpleProxyIDispatch(mock[ClientRemoting], proxyId, name)
}
