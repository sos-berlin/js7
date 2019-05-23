package com.sos.jobscheduler.base.utils

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.utils.Base64UUID.{base64ToUUID, uuidToBase64}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.util.UUID
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.prop.PropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class Base64UUIDTest extends FreeSpec
{
  "JSON" in {
    testJson(Base64UUID(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")), """"ABEiM0RVZneImaq7zN3u_w"""")
  }

  "Base64UUID" in {
    assert(Base64UUID(UUID.fromString("00000000-0000-0000-0000-000000000000")).string == "AAAAAAAAAAAAAAAAAAAAAA")
    assert(Base64UUID(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")).string == "ABEiM0RVZneImaq7zN3u_w")
  }

  "uuidToBase64" in {
    assert(uuidToBase64(UUID.fromString("00000000-0000-0000-0000-000000000000")) == "AAAAAAAAAAAAAAAAAAAAAA")
    assert(uuidToBase64(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")) == "ABEiM0RVZneImaq7zN3u_w")
    forAll(Gen.uuid)((uuid: UUID) => assert(base64ToUUID(uuidToBase64(uuid)) == Valid(uuid)))
  }
}
