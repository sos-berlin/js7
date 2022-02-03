package js7.base.utils

import java.util.UUID
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.utils.Base64UUID.{base64ToUUID, uuidToBase64}
import js7.tester.CirceJsonTester.testJson
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class Base64UUIDTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(Base64UUID(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")),
    json""" "ABEiM0RVZneImaq7zN3u_w" """)
  }

  "Base64UUID" in {
    assert(Base64UUID(UUID.fromString("00000000-0000-0000-0000-000000000000")).string == "AAAAAAAAAAAAAAAAAAAAAA")
    assert(Base64UUID(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")).string == "ABEiM0RVZneImaq7zN3u_w")
  }

  "uuidToBase64" in {
    forAll(Gen.uuid)((uuid: UUID) => assert(base64ToUUID(uuidToBase64(uuid)) == Right(uuid)))
  }

  "uuidToBase64 for testing values" in {
    assert(uuidToBase64(UUID.fromString("00000000-0000-0000-0000-000000000000")) == "AAAAAAAAAAAAAAAAAAAAAA")
    assert(Base64UUID.zero.string == "AAAAAAAAAAAAAAAAAAAAAA")

    assert(uuidToBase64(UUID.fromString("ffffffff-ffff-ffff-ffff-ffffffffffff")) == "_____________________w")
    assert(Base64UUID.ffff.string == "_____________________w")

    assert(uuidToBase64(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")) == "ABEiM0RVZneImaq7zN3u_w")
  }

  "random" in {
    val n = 10000
    assert(Set.fill(n)(Base64UUID.random()).size == n)
    assert(Set.fill(n)(Base64UUID.randomString()).size == n)
  }
}
