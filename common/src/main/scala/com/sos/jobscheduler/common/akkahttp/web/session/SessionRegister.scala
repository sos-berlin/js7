package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import com.sos.jobscheduler.data.session.SessionToken
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S] {

  private val sessions = new ConcurrentHashMap[SessionToken, S]()

  def add(session: S): SessionToken = {
    val token = SessionToken(SecretStringGenerator.newSecretString())
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
    object session extends Directive1[S] {
      def tapply(inner: Tuple1[S] ⇒ Route) =
        optionalHeaderValueByName(SessionToken.HeaderName) {
          case Some(string) ⇒
            val sessionToken = SessionToken(SecretString(string))
            sessions.get(sessionToken) match {
              case null ⇒
                complete(Forbidden → "Unknown SessionToken")  // 'Unauthorized' requires 'WWW-Authenticate'
              case session ⇒
                inner(Tuple1(session))
            }
          case None ⇒
            reject
        }
    }

    /**
      * Directive passes with known SessionToken or when no SessionToken header is set.
      */
    object optionalSession extends Directive1[Option[S]] {
      def tapply(inner: Tuple1[Option[S]] ⇒ Route) =
        optionalHeaderValueByName(SessionToken.HeaderName) {
          case Some(string) ⇒
            val sessionToken = SessionToken(SecretString(string))
            sessions.get(sessionToken) match {
              case null ⇒
                complete(Forbidden → "Unknown SessionToken") // 'Unauthorized' requires 'WWW-Authenticate'
              case session ⇒
                inner(Tuple1(Some(session)))
            }
          case None ⇒
            inner(Tuple1(None))
        }
    }
  }
}
