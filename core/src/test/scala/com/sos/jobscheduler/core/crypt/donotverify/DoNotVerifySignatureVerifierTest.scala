package com.sos.jobscheduler.core.crypt.donotverify

import cats.data.Validated.Valid
import com.sos.jobscheduler.data.crypt.GenericSignature
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class DoNotVerifySignatureVerifierTest extends FreeSpec
{
  "test" in {
    assert(DoNotVerifySignatureVerifier.checked(Nil).toOption.get eq DoNotVerifySignatureVerifier)
    assert(DoNotVerifySignatureVerifier.checked(Seq(1)).isInvalid)
    assert(DoNotVerifySignatureVerifier.genericSignatureToSignature(GenericSignature("XX", "YY")) eq DoNotVerifySignature)
    assert(DoNotVerifySignatureVerifier.verify("MESSAGE", DoNotVerifySignature) == Valid(Nil))
  }
}
