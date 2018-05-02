import java.time.Instant
import java.time.format.DateTimeFormatter.ISO_INSTANT

object VersionFormatter
{
  private val CommitHashLength = 7

  def buildVersion(version: String, versionCommitHash: Option[String], branch: String): String = {
    var result = version
    val resolvedBranch = branch match {
      case o if o.isEmpty || versionCommitHash.getOrElse("").startsWith(o) ⇒
        sys.env.getOrElse("GIT_BRANCH", "")  // Maybe set by Jenkins Git plugin
      case o ⇒ o
    }
    if (version endsWith "-SNAPSHOT") result += " " + branchAndCommitSuffix(resolvedBranch, versionCommitHash map (_ take CommitHashLength))
    result.trim
  }

  private def branchAndCommitSuffix(branch: String, versionCommitHash: Option[String]) = {
    val parts = branch +:
      (versionCommitHash map { _ take 7 }) ++:
      List(ISO_INSTANT.format(Instant.now).take(16) + "Z")
    parts filter { _.nonEmpty } mkString ("(", " ", ")")
  }
}
