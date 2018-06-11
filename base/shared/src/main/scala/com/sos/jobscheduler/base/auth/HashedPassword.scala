package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.auth.HashedPassword._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.generic.SecretString.timingAttackSecureEqual
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final case class HashedPassword(hashed: SecretString, hasher: String ⇒ String)
{
  def equalsClearText(clear: SecretString) = timingAttackSecureEqual(hashed.string, hasher(clear.string))

  def hashAgainRandom: HashedPassword = {
    val salt = Random.nextString(RehashSaltLength)
    def hashAgain(string: String) = sha(string + salt)
    HashedPassword(SecretString(hashAgain(hashed.string)), hasher andThen hashAgain)
  }

  override def toString = "HashedPassword"
}

object HashedPassword
{
  private val RehashSaltLength = 20
  /** No clear-text password matches this unknown password. */
  val MatchesNothing = HashedPassword(SecretString("MatchesNothing"), _ ⇒ "")

  /** The empty clear-text password, differently hashed at each invocation. */
  def newEmpty = HashedPassword(SecretString(""), identity).hashAgainRandom

  private def sha(string: String) = {
    val digest = MessageDigest.getInstance("SHA-256")  // Not thread-safe
    Base64.getUrlEncoder.encodeToString(digest.digest(string.getBytes(UTF_8)))  // Convert bytes to String
  }
}
