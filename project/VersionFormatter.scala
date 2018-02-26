import java.time.Instant
import java.time.format.DateTimeFormatter.ISO_INSTANT

object VersionFormatter  {

  def buildVersion(version: String, versionCommitHash: Option[String], branch: String): String = {
    var result = version
    if (version endsWith "-SNAPSHOT") result += " " + branchAndCommitSuffix(resolveBranch(branch), versionCommitHash)
    result.trim
  }

  private def resolveBranch(branch: String) =
    branch match {
      case "" ⇒ sys.env.getOrElse("GIT_BRANCH", "")  // Jenkins Git plugin
      case o ⇒ o
    }

  private def branchAndCommitSuffix(branch: String, versionCommitHash: Option[String]) = {
    val parts = branch +:
      (versionCommitHash map { _ take 7 }) ++:
      List(ISO_INSTANT.format(Instant.now).take(16) + "Z")
    parts filter { _.nonEmpty } mkString ("(", " ", ")")
  }
}
