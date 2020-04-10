package com.sos.jobscheduler.common.auth

import com.sos.jobscheduler.base.auth.{SimpleUser, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.auth.IdToUser.RawUserAccount
import com.sos.jobscheduler.common.auth.IdToUserTest._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.typesafe.config.ConfigFactory
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IdToUserTest extends AnyFreeSpec
{
  "Unknown user" in {
    assert(idToUser(UserId("UNKNOWN")) == None)
  }

  "Plain password" in {
    val Some(u) = idToUser(PlainUserId)
    assert(u.id == PlainUserId)
    assert(u.hashedPassword equalsClearText PlainPassword)
  }

  "SHA512 hashed password" in {
    val Some(u) = idToUser(Sha512UserId)
    assert(u.id == Sha512UserId)
    assert(u.hashedPassword equalsClearText Sha512Password)
  }

  "fromConfig" - {
    "No jobscheduler.auth.users" in {
      intercept[com.typesafe.config.ConfigException.Missing] {
        IdToUser.fromConfig(ConfigFactory.parseString(""), SimpleUser.apply)
      }
      //assert(idToUser(UserId("UNKNOWN")) == None)
    }

    val idToUser = IdToUser.fromConfig(
      ConfigFactory.parseString(
        """jobscheduler.auth.users {
          |  A = "plain:PLAIN-PASSWORD"
          |  B = "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2"
          |  C {
          |    password = "plain:PLAIN-PASSWORD"
          |  }
          |}""".stripMargin),
      SimpleUser.apply)

    "fromConfig" in {
      val Some(a) = idToUser(UserId("A"))
      val Some(b) = idToUser(UserId("B"))
      val Some(c) = idToUser(UserId("C"))
      assert(a.id == UserId("A"))
      assert(b.id == UserId("B"))
      assert(c.id == UserId("C"))
      assert(a.hashedPassword equalsClearText PlainPassword)
      assert(b.hashedPassword equalsClearText Sha512Password)
      assert(c.hashedPassword equalsClearText PlainPassword)
    }

    "thread-safe" in {
      val n = 10000
      val a = Future.sequence(for (i <- 1 to n) yield
        Future { assert(idToUser(UserId("A")).get.hashedPassword equalsClearText PlainPassword, s"#$i identity") })
      val b = Future.sequence((1 to n).map(i =>
        Future { assert(idToUser(UserId("B")).get.hashedPassword equalsClearText Sha512Password, s"#$i SHA-512") }))
      List(a, b) await 99.s
    }
  }
}

private object IdToUserTest
{
  private val PlainUserId = UserId("PLAIN-USER")
  private val PlainPassword = SecretString("PLAIN-PASSWORD")
  private val Sha512UserId = UserId("SHA512-USER")
  private val Sha512Password = SecretString("SHA512-PASSWORD")
  private val PlainConfiguredPassword = SecretString(s"plain:${PlainPassword.string}")
  private val Sha512ConfiguredPassword = SecretString(
    "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2")

  private val TestConfigValidator = ConfigFactory.parseMap(Map(
    PlainUserId.string -> PlainConfiguredPassword.string,
    Sha512UserId.string -> Sha512ConfiguredPassword.string).asJava)

  private val idToUser = new IdToUser(
    userId => TestConfigValidator.optionAs[SecretString](userId.string).map(o => RawUserAccount(o, Set.empty)),
    SimpleUser.apply,
    toPermission = Map.empty)
}
