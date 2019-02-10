package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.crypt.donotverify.DoNotVerifySignatureVerifier
import com.sos.jobscheduler.core.filebased.FileBasedVerifier
import com.sos.jobscheduler.data.master.MasterFileBaseds

/**
  * @author Joacim Zschimmer
  */
package object configuration
{
  val DoNotVerifyMasterFileBasedVerifier = new FileBasedVerifier(DoNotVerifySignatureVerifier, MasterFileBaseds.jsonCodec)
}
