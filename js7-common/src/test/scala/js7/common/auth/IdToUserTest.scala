package js7.common.auth

import com.typesafe.config.ConfigFactory
import js7.base.auth.{DistinguishedName, SimpleUser, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.common.auth.IdToUser.RawUserAccount
import js7.common.auth.IdToUserTest.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final class IdToUserTest extends OurTestSuite
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
    "No js7.auth.users" in {
      intercept[com.typesafe.config.ConfigException.Missing] {
        IdToUser.fromConfig(ConfigFactory.parseString(""), SimpleUser.apply)
      }
      //assert(idToUser(UserId("UNKNOWN")) == None)
    }

    val idToUser = IdToUser.fromConfig(
      config"""
        js7.auth.users {
          A = "plain:PLAIN-PASSWORD"
          B = "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2"
          B1 = "sha512:130C7809C9E5A8D81347B55F5C82C3A7407F4B41B461EB641887D276B11AF4B575C5A32D1CF104E531C700E4B1DDD75B27B9E849576F6DFB8CA42789FBC7ECE2"
          C {
            password = "plain:PLAIN-PASSWORD"
          }
          D {
            password = "plain:PLAIN-PASSWORD"
            distinguished-names = [ "CN=IdToUserTest", "CN=D" ]
          }
          E1 {
            distinguished-names = [ "CN=E" ]
          }
          E2 {
            distinguished-names = [ "CN=E" ]
          }
          EMPTY-PASSWORD {
            password = ""
          }
        }""",
      SimpleUser.apply)

    "fromConfig" in {
      val Some(a) = idToUser(UserId("A"))
      val Some(b) = idToUser(UserId("B"))
      val Some(b1) = idToUser(UserId("B1"))
      val Some(c) = idToUser(UserId("C"))
      val Some(d) = idToUser(UserId("D"))
      val Some(e1) = idToUser(UserId("E1"))
      val Some(e2) = idToUser(UserId("E2"))
      assert(a.id == UserId("A"))
      assert(b.id == UserId("B"))
      assert(b1.id == UserId("B1"))
      assert(c.id == UserId("C"))
      assert(d.id == UserId("D"))
      assert(e1.id == UserId("E1"))
      assert(e2.id == UserId("E2"))
      assert(a.hashedPassword equalsClearText PlainPassword)
      assert(b.hashedPassword equalsClearText Sha512Password)
      assert(b1.hashedPassword equalsClearText Sha512Password)
      assert(c.hashedPassword equalsClearText PlainPassword)
      assert(d.hashedPassword equalsClearText PlainPassword)
      assert(d.distinguishedNames == List(DistinguishedName("CN=IdToUserTest"), DistinguishedName("CN=D")))
      assert(!e1.hashedPassword.equalsClearText(PlainPassword))
      assert(!e2.hashedPassword.equalsClearText(PlainPassword))
      assert(e1.distinguishedNames == List(DistinguishedName("CN=E")))
      assert(e2.distinguishedNames == List(DistinguishedName("CN=E")))

      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN=IdToUserTest")) == Right(Right(d)))
      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN=IdToUserTest")).toOption.get.toOption.get eq d)
      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN = IdToUserTest")) == Right(Right(d)))
      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN = IdToUserTest")) == Right(Right(d)))
      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN=D")) == Right(Right(d)))
      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN=E")) == Right(Left(Set(e1.id, e2.id))))
      assert(idToUser.distinguishedNameToIdsOrUser(DistinguishedName("CN=UNKNOWN")) == Left(Problem("Unknown distinguished name 'CN=UNKNOWN'")))

      val Some(emptyPasswordUser) = idToUser(UserId("EMPTY-PASSWORD"))
      assert(emptyPasswordUser.hashedPassword equalsClearText SecretString(""))
    }

    "thread-safe" in {
      val n = 10000
      val a = Future.sequence(
        for (i <- 1 to n) yield
          Future { assert(idToUser(UserId("A")).get.hashedPassword equalsClearText PlainPassword, s"#$i identity") })
      val b = Future.sequence(
        for (i <- 1 to n) yield
          Future { assert(idToUser(UserId("B")).get.hashedPassword equalsClearText Sha512Password, s"#$i SHA-512") })
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
    userId => TestConfigValidator.optionAs[SecretString](userId.string).map(o => RawUserAccount(userId, Some(o))),
    distinguishedNameToUserIds = Map.empty,
    SimpleUser.apply,
    toPermission = Map.empty)
}
