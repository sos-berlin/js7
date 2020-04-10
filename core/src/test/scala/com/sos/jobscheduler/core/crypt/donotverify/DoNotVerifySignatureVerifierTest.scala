package com.sos.jobscheduler.core.crypt.donotverify

import com.sos.jobscheduler.base.utils.CatsUtils.bytesToInputStreamResource
import com.sos.jobscheduler.data.crypt.GenericSignature
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class DoNotVerifySignatureVerifierTest extends AnyFreeSpec
{
  "test" in {
    assert(DoNotVerifySignatureVerifier.checked(Nil).toOption.get eq DoNotVerifySignatureVerifier)
    assert(DoNotVerifySignatureVerifier.checked(bytesToInputStreamResource(Array(1.toByte)) :: Nil).isLeft)
    assert(DoNotVerifySignatureVerifier.genericSignatureToSignature(GenericSignature("XX", "YY")) eq DoNotVerifySignature)
    assert(DoNotVerifySignatureVerifier.verify("MESSAGE", DoNotVerifySignature) == Right(Nil))
  }
}
