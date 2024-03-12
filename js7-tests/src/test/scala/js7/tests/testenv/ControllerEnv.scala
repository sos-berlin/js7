package js7.tests.testenv

import cats.effect.Resource
import com.typesafe.config.ConfigUtil.quoteString
import java.nio.file.Path
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.crypt.SignatureVerifier
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.journal.data.JournalLocation
import js7.tests.testenv.DirectoryProvider.{AgentTrustStoreResource, defaultVerifier}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import scala.collection.immutable.Iterable

/** Environment with config and data directories for a Controller. */
final class ControllerEnv(
  val directory: Path,
  protected val verifier: SignatureVerifier = defaultVerifier,
  keyStore: Option[JavaResource],
  trustStores: Iterable[JavaResource],
  agentHttpsMutual: Boolean)
extends ProgramEnv.WithFileJournal:
  type Program = RunningController

  protected type S = ControllerState
  val S = ControllerState

  protected def confFilename = "controller.conf"

  def programResource(using IORuntime): Resource[IO, RunningController] = ???

  val journalLocation = JournalLocation(ControllerState, stateDir / "controller")
  val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))

  initialize()

  protected override def createDirectoriesAndFiles(): Unit =
    super.createDirectoriesAndFiles()
    for keyStore <- keyStore do
      configDir / "private/private.conf" ++= """
       |js7.web.https.keystore {
       |  store-password = "jobscheduler"
       |  key-password = "jobscheduler"
       |}
       |""".stripMargin
      configDir / "private/https-keystore.p12" := keyStore.contentBytes
      provideTrustStore(AgentTrustStoreResource, "agent-https-truststore.p12")
      for (o, i) <- trustStores.zipWithIndex do
        provideTrustStore(o, s"extra-${i + 1}-https-truststore.p12")

  private def provideTrustStore(resource: JavaResource, filename: String): Unit =
    val trustStore = configDir / "private" / filename
    trustStore := resource.contentBytes
    configDir / "private/private.conf" ++= s"""
     |js7.auth.users.${userAndPassword.userId.string}.password = "plain:${userAndPassword.password.string}"
     |js7.web.https.truststores += {
     |  file = ${quoteString(trustStore.toString)}
     |  store-password = "jobscheduler"
     |}
     |""".stripMargin

  def writeAgentAuthentication(env: DirectorEnv): Unit =
    writeAgentAuthentication(env.agentPath, env.controllerPassword)

  def writeAgentAuthentication(agentPath: AgentPath, password: SecretString): Unit =
    if !agentHttpsMutual then
      val quotedAgentPath = quoteString(agentPath.string)
      val quotedPassword = quoteString(password.string)
      privateConf ++= s"js7.auth.agents.$quotedAgentPath = $quotedPassword\n"
    else {
      // Agent uses the distinguished name of the Controller's HTTPS certificate
    }
