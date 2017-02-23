package com.sos.scheduler.engine.common.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.EncodedPasswordValidatorTest._
import com.sos.scheduler.engine.common.configutils.Configs.ConvertibleConfig
import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
final class EncodedPasswordValidatorTest extends FreeSpec {

  "Unknown user" in {
    assert(Validator.hashedPasswordOption(UserId("UNKNOWN")) == None)
    assert(!Validator(UserAndPassword(UserId("UNKNOWN"), SecretString("WRONG-PASSWORD"))))
  }

  "Wrong password" in {
    assert(!Validator.validatePassword(PlainUserId → SecretString("WRONG-PASSWORD"))(PlainConfiguredPassword))
    assert(!Validator.validatePassword(Sha512UserId → SecretString("WRONG-PASSWORD"))(Sha512ConfiguredPassword))
    assert(!Validator(PlainUserId → SecretString("WRONG-PASSWORD")))
    assert(!Validator(Sha512UserId → SecretString("WRONG-PASSWORD")))
  }

  "Plain password" in {
    assert(Validator.hashedPasswordOption(PlainUserId) contains SecretString("plain:PLAIN-PASSWORD"))
    assert(Validator.validatePassword(PlainUserId → SecretString("PLAIN-PASSWORD"))(PlainConfiguredPassword))
    assert(Validator(PlainUserId → SecretString("PLAIN-PASSWORD")))
  }

  "SHA512 hashed password" in {
    assert(Validator.validatePassword(Sha512UserId → SecretString("SHA512-PASSWORD"))(Sha512ConfiguredPassword))
    assert(Validator(Sha512UserId → SecretString("SHA512-PASSWORD")))
  }
}

private object EncodedPasswordValidatorTest {
  private val PlainUserId = UserId("PLAIN-USER")
  private val Sha512UserId = UserId("SHA512-USER")
  private val PlainConfiguredPassword = SecretString("plain:PLAIN-PASSWORD")
  private val Sha512ConfiguredPassword = SecretString(  // "SHA512-PASSWORD"
    "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2")

  private val TestConfigValidator = ConfigFactory.parseMap(Map(
    PlainUserId.string → PlainConfiguredPassword.string,
    Sha512UserId.string → Sha512ConfiguredPassword.string))

  private val Validator = new EncodedPasswordValidator(userId ⇒ TestConfigValidator.optionAs[SecretString](userId.string))
}
