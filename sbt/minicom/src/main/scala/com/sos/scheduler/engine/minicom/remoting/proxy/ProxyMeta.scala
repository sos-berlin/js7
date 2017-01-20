package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.minicom.types.CLSID

/**
  * Like the C++ class Reference_with_properties.
  * Provides meta properties for the proxy on the peer side.
  * `ProxyMetaData` is sent to the peer side at first use
  * and may be used for proxy optimization.
  *
  * @author Joacim Zschimmer
  */
final case class ProxyMeta(
  clsid: CLSID,
  properties: Map[String, Any])
