package com.sos.jobscheduler.master.javaapi

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.master.{MasterRepoReader ⇒ ScalaMasterRepoReader}
import java.nio.file.Path
import java.util.Optional
import scala.collection.JavaConverters._

/**
  * Java variant of [[com.sos.jobscheduler.master.MasterRepoReader]].
  * @param isLastVersionOnly Returned Repo contains only the last version of each FileBased.
  * @author Joacim Zschimmer
  */
final class MasterRepoReader private(fileBasedDirectory: Path, isLastVersionOnly: Boolean)
{
  def this(fileBasedDirectory: Path) = this(fileBasedDirectory, true)

  private val masterRepoReader = new ScalaMasterRepoReader(fileBasedDirectory)

  def applyConfigurationDirectory(repo: Repo): Object =
    applyConfigurationDirectory(repo, Optional.empty())

  def applyConfigurationDirectory(repo: Repo, versionId: Optional[String]): Object = {
    val versionId_ = Option(versionId orElse null) map VersionId.apply
    masterRepoReader.applyConfigurationDirectory(repo, versionId_) match {
      case Valid(repo: Repo) ⇒
        adaptRepo(repo)

      case Invalid(Problem.Multiple(problems)) ⇒
        problems.toVector.asJava: java.util.List[Problem]

      case Invalid(problem: Problem) ⇒
        (problem :: Nil).asJava: java.util.List[Problem]
    }
  }

  def keepLastVersionOnly: MasterRepoReader =
    new MasterRepoReader(fileBasedDirectory, isLastVersionOnly = true)

  private def adaptRepo(repo: Repo): Repo =
    if (isLastVersionOnly)
      repo.onlyCurrentVersion
    else
      repo
}
