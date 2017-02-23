import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object VersionFormatter  {

  private val BuildDateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

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
      List(BuildDateTimeFormatter.format(ZonedDateTime.now))
    parts filter { _.nonEmpty } mkString ("(", " ", ")")
  }
}
