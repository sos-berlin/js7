package com.sos.jobscheduler.minicom.idispatch

import com.sos.jobscheduler.minicom.types.{CLSID, IID, IUnknown}

/**
 * @author Joacim Zschimmer
 */
trait IUnknownFactory {
  def clsid: CLSID
  def iid: IID
  def newIUnknown(): IUnknown
}
