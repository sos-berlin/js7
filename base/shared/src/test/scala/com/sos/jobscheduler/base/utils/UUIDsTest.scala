package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.UUIDs.{base64ToUUID, uuidToBase64}
import java.util.UUID
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.prop.PropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class UUIDsTest extends FreeSpec
{
  "uuidToBase64" in {
    assert(uuidToBase64(UUID.fromString("00000000-0000-0000-0000-000000000000")) == "AAAAAAAAAAAAAAAAAAAAAA")
    assert(uuidToBase64(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")) == "ABEiM0RVZneImaq7zN3u_w")
    forAll(Gen.uuid)((uuid: UUID) => assert(base64ToUUID(uuidToBase64(uuid)) == uuid))
  }
}
