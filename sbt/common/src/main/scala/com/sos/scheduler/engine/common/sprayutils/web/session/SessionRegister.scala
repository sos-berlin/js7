package com.sos.scheduler.engine.common.sprayutils.web.session

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.SecretStringGenerator
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.completeWithError
import com.sos.scheduler.engine.data.session.SessionToken
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._
import shapeless.{::, HNil}
import spray.http.StatusCodes.Forbidden
import spray.routing.Directives._
import spray.routing.{Directive1, Route}

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[A](newSession: (SessionToken) ⇒ A) {

  private val sessions = new ConcurrentHashMap[SessionToken, A]()

  def newSessionToken(): SessionToken = {
    val token = SessionToken(SecretStringGenerator.newSecretString())
    val session = newSession(token)
    assert(!sessions.isDefinedAt(token))
    sessions += token → session
    token
  }

  def remove(sessionToken: SessionToken): Unit = {
    sessions -= sessionToken
  }

  def contains(sessionToken: SessionToken): Boolean =
    sessions isDefinedAt sessionToken

  object directives {
    object session extends Directive1[A] {
      def happly(inner: A :: HNil ⇒ Route) =
        optionalHeaderValueByName(SessionToken.HeaderName) {
          case Some(string) ⇒
            val sessionToken = SessionToken(SecretString(string))
            sessions.get(sessionToken) match {
              case null ⇒
                completeWithError(Forbidden, "Unknown SessionToken")  // 'Unauthorized' requires 'WWW-Authenticate'
              case session ⇒
                inner(session :: HNil)
            }
          case None ⇒
            reject
        }
    }

    /**
      * Directive passes with known SessionToken or when no SessionToken header is set.
      */
    object optionalSession extends Directive1[Option[A]] {
      def happly(inner: Option[A] :: HNil ⇒ Route) =
        optionalHeaderValueByName(SessionToken.HeaderName) {
          case Some(string) ⇒
            val sessionToken = SessionToken(SecretString(string))
            sessions.get(sessionToken) match {
              case null ⇒
                //reject(AuthenticationFailedRejection(CredentialsMissing, ...))
                completeWithError(Forbidden, "Unknown SessionToken") // 'Unauthorized' requires 'WWW-Authenticate'
              case session ⇒
                inner(Some(session) :: HNil)
            }
          case None ⇒
            inner(None :: HNil)
        }
    }
  }
}

object SessionRegister {
  def forTest = new SessionRegister(_ ⇒ ())
}
