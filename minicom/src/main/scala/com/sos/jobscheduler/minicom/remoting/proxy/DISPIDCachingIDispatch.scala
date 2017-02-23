package com.sos.jobscheduler.minicom.remoting.proxy

import com.sos.jobscheduler.common.scalautil.ScalaConcurrentHashMap
import com.sos.jobscheduler.minicom.idispatch.{DISPID, IDispatch}

/**
  * @author Joacim Zschimmer
  */
trait DISPIDCachingIDispatch extends IDispatch {

  private val nameToDispId = new ScalaConcurrentHashMap[String, DISPID]

  abstract override def getIdOfName(name: String) = {
    val lName = name.toLowerCase
    nameToDispId.get(lName) getOrElse {
      val id = super.getIdOfName(name)
      nameToDispId.delegate.putIfAbsent(lName, id)
      id
    }
  }
}
