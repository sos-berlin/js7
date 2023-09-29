package js7.tests.https

import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import monix.execution.Scheduler.Implicits.traced

final class SelectWrongAliasHttpsTest extends ControllerHttpsStandardTests:
  override protected def useCluster = false
  override protected def agentHttpsMutual = true
  override protected def provideAgentClientCertificate = true
  override protected def controllerHttpsMutual = true
  override protected def provideControllerClientCertificate = true

  override protected def clientKeyAlias = Some("WRONG ALIAS")

  "Login" in:
    val t = intercept[IllegalArgumentException](
      httpControllerApi.login_(standardUserAndPassword) await 99.s)
    assert(t.getMessage startsWith "Unknown alias=WRONG ALIAS")
