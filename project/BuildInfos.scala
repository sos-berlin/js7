import com.github.sbt.git.GitPlugin.autoImport.git
import java.nio.ByteBuffer
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, OffsetDateTime}
import java.util.{Base64, UUID}
import sbt.Def
import sbt.Keys.{isSnapshot, version}
import scala.collection.immutable.ListMap

object BuildInfos
{
  private val CommitHashLength = 7
  private val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString _
  private val now = Instant.now()

  val versionIsTagged = Def.setting {
    git.gitCurrentTags.value.contains("v" + version.value)
  }

  private val shortCommitHash: Def.Initialize[String] =
    Def.setting(
      git.gitHeadCommit.value.filter(_.nonEmpty)
        .orElse(sys.env.get("GIT_COMMIT" /*Jenkins?*/))
        .getOrElse("")
        .take(CommitHashLength))

  private val branch = Def.setting {
    val branch = git.gitCurrentBranch.value
    if (branch.isEmpty || shortCommitHash.value.startsWith(branch))
      sys.env.getOrElse("GIT_BRANCH", "") // Maybe set by Jenkins Git plugin
    else
      branch
  }

  private val isUncommitted = Def.setting {
    git.gitUncommittedChanges.value || git.gitHeadCommit.value.isEmpty/*no Git?*/
  }

  /** Git commit date as Instant". */
  lazy val committedAt: Def.Initialize[Option[Instant]] =
    Def.setting(
      git.gitHeadCommitDate.value.map(parseInstant))

  lazy val info: Def.Initialize[Info] = Def.setting {
    val versionIsTagged = git.gitCurrentTags.value.contains("v" + version.value)
    if (isUncommitted.value) {
      val info = new Uncommitted(version.value, branch = branch.value,
        commitHash = git.gitHeadCommit.value.getOrElse(""))
      if (isUncommitted.value && !info.isSnapshot) println(
        s"❓ Uncommitted files but version does not ends with -SNAPSHOT: ${version.value} ❓")
      info
    } else if (!versionIsTagged || isSnapshot.value) {
      val info = new Untagged(version.value, branch = branch.value,
        committedAt = committedAt.value, commitHash = shortCommitHash.value)
      if (!info.isSnapshot) println(s"❗ Commit is not tagged with v${version.value} ❗")
      info
    } else
      new Tagged(version.value, branch = branch.value,
        commitHash = shortCommitHash.value)
  }

  sealed trait Info {
    /** Version as configured in source.
     * Taken from the version.sbt file in root directory. */
    def version: String

    /** The version, maybe with a suffix consisting of "+", timestamp and commit hash.
     * This is the relevant and unique version number. */
    def longVersion: String

    /** longVersion, maybe appended with a space and the branch name. */
    def prettyVersion: String
    def commitHash: String

    /** version or a random number. */
    def buildId: String

    final lazy val buildInfoMap = ListMap[String, Any](
      "version" -> version,
      "longVersion" -> longVersion,
      "prettyVersion" -> prettyVersion,
      "buildId" -> buildId,
      "commitId" -> commitHash,
      // TODO The version of the Java compiler would be more appropriate:
      "javaVersion" -> sys.props.getOrElse("java.version", ""),
      "javaRuntimeVersion" -> sys.props.getOrElse("java.runtime.version", ""),
      "catsEffectVersion" -> Dependencies.catsEffectVersion)

    final lazy val buildPropertiesString: String =
      buildInfoMap
        .mapValues {
          case None => ""
          case Some(v) => v.toString.trim
          case v => v.toString.trim
        }
        .map { case (k, v) => s"build.$k=$v\n" }
        .mkString

    lazy val isSnapshot: Boolean =
      version.contains("-SNAPSHOT")
  }

  /** A committed and properly tagged version. */
  final class Tagged(val version: String, branch: String, val commitHash: String)
  extends Info {
    val longVersion: String =
      version

    val prettyVersion: String =
      longVersion

    val buildId: String =
      longVersion
  }

  sealed trait Branch {
    this: Info =>

    def branch: String

    private def branchSuffix =
      if (isSnapshot && (branch == "main" || releaseBranch.contains(branch)))
        ""
      else
        s" ($branch)"

    private def releaseBranch: Option[String] =
      version.indexOf('.', 2) match {
        case -1 => None
        case i => Some("release/" + version.take(i))
      }

    lazy final val prettyVersion =
      longVersion + branchSuffix
  }

  /** Version is not properly tagged or it's a SNAPSHOT version. */
  final class Untagged(
    val version: String, val
    branch: String,
    val committedAt: Option[Instant],
    val commitHash: String)
  extends Info with Branch {
    val longVersion =
      s"$version+${committedAt.fold("")(o => toSemverDate(o) + ".")}$commitHash"

    val buildId: String =
      longVersion
  }

  /** Uncommitted contains build time dependent values. Not for release versions. */
  final class Uncommitted(val version: String, val branch: String, val commitHash: String)
  extends Info with Branch {
    /** "2.0.0+UNCOMMITTED.20210127.120000" */
    val longVersion: String =
      version + "+" + toSemverDate(now) + ".UNCOMMITTED"

    val buildId: String = {
      val uuid = UUID.randomUUID
      val buffer = ByteBuffer.wrap(new Array[Byte](16))
      buffer.putLong(uuid.getMostSignificantBits)
      buffer.putLong(uuid.getLeastSignificantBits)
      toUrlBase64(buffer.array)
    }
  }

  private def toSemverDate(instant: Instant): String =
    instant
      .toString
      .filter(c => c != '-' && c != ':')
      .take(13) // 20231213T1200
      .replace('T', '.')

  private val instantFormatter = new DateTimeFormatterBuilder()
    .append(ISO_LOCAL_DATE_TIME)
    .appendPattern("XX")
    .toFormatter

  /** Parses 2019-01-14T12:00:00Z and 2019-01-14T13:00:00+01:00. */
  private def parseInstant(s: String) =
    OffsetDateTime.parse(s, instantFormatter).toInstant
}
