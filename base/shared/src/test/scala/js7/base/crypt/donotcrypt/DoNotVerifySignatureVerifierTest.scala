package js7.base.crypt.donotcrypt

import js7.base.crypt.GenericSignature
import js7.base.utils.CatsUtils.bytesToInputStreamResource
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
