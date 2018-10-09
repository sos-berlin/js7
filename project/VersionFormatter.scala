object VersionFormatter
{
  private val CommitHashLength = 7

  def buildVersion(version: String, versionCommitHash: Option[String], commitDate: Option[String], branch: String, isUncommitted: Boolean): String = {
    val sb = new StringBuilder
    sb ++= version
    if (version endsWith "-SNAPSHOT") {
      val resolvedBranch = branch match {
        case o if o.isEmpty || versionCommitHash.getOrElse("").startsWith(o) ⇒
          sys.env.getOrElse("GIT_BRANCH", "")  // Maybe set by Jenkins Git plugin
        case o ⇒ o
      }
      sb ++= " "
      sb ++= branchAndCommitSuffix(resolvedBranch, versionCommitHash, commitDate)
    }
    if (isUncommitted) sb ++= " UNCOMMITTED"
    sb.toString.trim
  }

  private def branchAndCommitSuffix(branch: String, versionCommitHash: Option[String], commitDate: Option[String]) =
    (branch +: versionCommitHash.map(_ take CommitHashLength) ++: commitDate.toList)
      .filter(_.nonEmpty)
      .mkString("(", " ", ")")
}
