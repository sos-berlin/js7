package com.sos.jobscheduler.master.order

import cats.data.Validated.Invalid
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.filebased.{FileBasedReader, FileBaseds, Repo}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedVersion, RepoEvent, TypedPath}
import com.sos.jobscheduler.master.order.FileBasedConfiguration._
import java.nio.file.Path
import monix.eval.Coeval
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
private[order] trait FileBasedConfiguration
{
  protected def readers: Iterable[FileBasedReader]
  protected def directory: Path
  private var _repo = Repo.empty

  def repo = _repo

  def replaceRepo(changed: Repo): Unit = {
    _repo = changed
  }

  protected def readConfiguration(version: FileBasedVersion): Checked[(Seq[RepoEvent], Coeval[Unit])] =
    for {
      events ← FileBaseds.readDirectory(readers, directory, repo.currentFileBaseds, version)
      sideEffect ← eventsToSideEffect(events)  // May return DuplicateVersionProblem
    } yield (events, sideEffect)

  def recover(events: Seq[RepoEvent]): Unit =
    eventsToSideEffect(events).force()

  protected def eventsToSideEffect(events: Seq[RepoEvent]): Checked[Coeval[Unit]] =
    for {
      changedRepo ← repo.applyEvents(events)
      diffSideEffect ← handleDiff(FileBaseds.Diff.fromEvents(events))
    } yield Coeval {
      replaceRepo(changedRepo)
      diffSideEffect()
    }

  protected def updateFileBaseds(diff: FileBaseds.Diff[TypedPath, FileBased]): Seq[Checked[Coeval[Unit]]]

  protected def handleDiff(diff: FileBaseds.Diff[TypedPath, FileBased]): Checked[Coeval[Unit]] = {
    updateFileBaseds(diff).toVector.sequence map foldSideEffects
  }

  protected def onlyAdditionPossible[P <: TypedPath, A <: FileBased](diff: FileBaseds.Diff[P, A]): Seq[Invalid[Problem]] =
    diff.deleted.map(o ⇒ Invalid(Problem(s"Deletion of configuration file is not supported: $o"))) ++:
      diff.changed.map(o ⇒ Invalid(Problem(s"Change of configuration file is not supported: ${o.path}")))
}

object FileBasedConfiguration {
  private def foldSideEffects(sideEffects: Iterable[Coeval[Unit]]): Coeval[Unit] =
    Coeval {
      for (o ← sideEffects) yield o()
    }
}
