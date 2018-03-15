package com.sos.jobscheduler.master.order

import cats.data.Validated.Invalid
import cats.effect.IO
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.filebased.{FileBasedReader, FileBaseds, Repo}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath, VersionId}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
private[order] trait FileBasedConfigurationReader
{
  protected def readers: Iterable[FileBasedReader]
  protected def directory: Path

  protected def readConfiguration(repo: Repo, versionId: VersionId): Checked[(Seq[RepoEvent], Repo, IO[Unit])] =
    for {
      events ← FileBaseds.readDirectory(readers, directory, repo.currentFileBaseds, versionId)
      changedRepo ← repo.applyEvents(events)  // May return DuplicateVersionProblem
      hd ← handleDiff(FileBaseds.Diff.fromEvents(events))
    } yield (events, changedRepo, hd)

  protected def updateFileBaseds(diff: FileBaseds.Diff[TypedPath, FileBased]): Seq[Checked[IO[Unit]]]

  protected def handleDiff(diff: FileBaseds.Diff[TypedPath, FileBased]): Checked[IO[Unit]] = {
    updateFileBaseds(diff).toVector.sequence map (_.fold(IO.unit)(_ >> _))
  }

  protected def onlyAdditionPossible[P <: TypedPath, A <: FileBased](diff: FileBaseds.Diff[P, A]): Seq[Invalid[Problem]] =
    diff.deleted.map(o ⇒ Invalid(Problem(s"Deletion of configuration file is not supported: $o"))) ++:
      diff.changed.map(o ⇒ Invalid(Problem(s"Change of configuration file is not supported: ${o.path}")))
}
