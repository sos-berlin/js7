package com.sos.jobscheduler.common.auth

import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.auth.EncodedToHashedPassword.sha512Hasher
import com.sos.jobscheduler.common.auth.EncodedToHashedPassword.identityHasher
import com.sos.jobscheduler.common.auth.EncodedToHashedPasswordTest._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
final class EncodedToHashedPasswordTest extends FreeSpec {

  "Unknown user" in {
    assert(encodedToHashedPassword(UserId("UNKNOWN")) == None)
  }

  "Plain password" in {
    assert(encodedToHashedPassword(PlainUserId) == Some(HashedPassword(SecretString(PlainPassword), identityHasher)))
  }

  "SHA512 hashed password" in {
    assert(encodedToHashedPassword(Sha512UserId) == Some(HashedPassword(SecretString(sha512Hasher(Sha512Password)), sha512Hasher)))
  }
}

private object EncodedToHashedPasswordTest {
  private val PlainUserId = UserId("PLAIN-USER")
  private val PlainPassword = "PLAIN-PASSWORD"
  private val Sha512UserId = UserId("SHA512-USER")
  private val Sha512Password = "SHA512-PASSWORD"
  private val PlainConfiguredPassword = SecretString(s"plain:$PlainPassword")
  private val Sha512ConfiguredPassword = SecretString(
    "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2")

  private val TestConfigValidator = ConfigFactory.parseMap(Map(
    PlainUserId.string → PlainConfiguredPassword.string,
    Sha512UserId.string → Sha512ConfiguredPassword.string))

  private val encodedToHashedPassword = new EncodedToHashedPassword(userId ⇒ TestConfigValidator.optionAs[SecretString](userId.string))
}
