package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.data.filebased.{RepoEvent, VersionId}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
trait RepoReader
{
  protected def readers: Iterable[FileBasedReader]
  protected def fileBasedDirectory: Path

  def applyConfigurationDirectory(repo: Repo, versionId: Option[VersionId]): Checked[Repo] =
    for (o ← readConfiguration(repo, versionId)) yield
      o._2

  def readConfiguration(repo: Repo, versionId: Option[VersionId]): Checked[(Seq[RepoEvent], Repo)] =
    for {
      events ← FileBaseds.readDirectory(readers, fileBasedDirectory, repo.currentFileBaseds, versionId getOrElse repo.newVersionId())
      changedRepo ← repo.applyEvents(events)  // May return DuplicateVersionProblem
    } yield (events, changedRepo)
}
