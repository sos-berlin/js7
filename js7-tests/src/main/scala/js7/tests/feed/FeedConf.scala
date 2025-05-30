package js7.tests.feed

import cats.syntax.option.*
import com.typesafe.config.ConfigFactory
import java.nio.file.Files.exists
import java.nio.file.Path
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.convert.AsJava.StringAsPath
import js7.base.crypt.DocumentSigner
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.utils.CatsUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.service.pgp.PgpSigner

private final case class FeedConf(
  admissions: Nel[Admission],
  documentSigner: Option[DocumentSigner],
  watchOrders: Boolean = false)
extends BasicConfiguration:
  val config = ConfigFactory.empty


private object FeedConf:

  private val defaultUserAndPassword = UserAndPassword(UserId("demo"), SecretString("demo")).some
  private val pgpPassword = SecretString("PGP-PASSWORD")

  def fromCommandLine(args: CommandLineArguments): FeedConf =
    val maybeConfigDir = args.optionAs[Path]("--config-directory=")

    val result = FeedConf(
      admissions =
        val userAndPassword = args.optionAs[String]("--user=")
          .fold(defaultUserAndPassword): string =>
            string.nonEmpty ?
              string.split(":", 2).match
                case Array(u, password) => UserAndPassword(UserId(u), SecretString(password))
                case Array(u) => UserAndPassword(UserId(u), SecretString.empty)
        Nel.unsafe(args.seqAs[Uri]("--controller=").map(Admission(_, userAndPassword))),

      documentSigner =
        maybeConfigDir.map(_ / "private" / "private-pgp-key.asc")
          .flatMap: privateKeyFile =>
            exists(privateKeyFile) ?
              PgpSigner.checked(privateKeyFile.byteArray, pgpPassword).orThrow,

      watchOrders = args.boolean("--watch-orders", false))

    args.requireNoMoreArguments()
    result
