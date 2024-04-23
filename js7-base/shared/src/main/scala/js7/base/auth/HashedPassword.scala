package js7.base.auth

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64
import js7.base.auth.HashedPassword.*
import js7.base.generic.SecretString
import js7.base.generic.SecretString.timingAttackSecureEqual
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
sealed case class HashedPassword(hashed: SecretString, hasher: String => String):

  def equalsClearText(clear: SecretString): Boolean =
    timingAttackSecureEqual(hashed.string, hasher(clear.string))

  def hashAgainRandom: HashedPassword =
    val salt = Random.nextString(RehashSaltLength)
    def hashAgain(string: String) = sha(string + salt)
    HashedPassword(SecretString(hashAgain(hashed.string)), hasher andThen hashAgain)

  override def toString = "HashedPassword"


object HashedPassword:
  private val RehashSaltLength = 20

  /** No clear-text password matches this unknown password. */
  private[auth] val MatchesNothingString = "MatchesNothing"
  object MatchesNothing extends HashedPassword(SecretString(MatchesNothingString), _ => ""):
    override def toString = "HashedPassword(MatchesNothing)"

  private val Empty = HashedPassword(SecretString.empty, identity)
  private val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString

  /** The empty clear-text password, differently hashed at each invocation. */
  def newEmpty(): HashedPassword =
    Empty.hashAgainRandom

  private def sha(string: String) =
    val digest = MessageDigest.getInstance("SHA-256")  // Not thread-safe
    toUrlBase64(digest.digest(string.getBytes(UTF_8)))  // Convert bytes to String
