package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.minicom.types.IUnknown

/**
  * Like the C++ class Has_reference_with_properties.
  * A marshallable IUnknown providing meta properties for the proxy on the peer side.
  *
  * @author Joacim Zschimmer
  */
trait HasProxyMeta {
  this: IUnknown ⇒

  def proxyMeta: ProxyMeta
}

object HasProxyMeta {
  //val iid = IID(UUID fromString "feee4703-6c1b-11d8-8103-000476ee8afb")
}
