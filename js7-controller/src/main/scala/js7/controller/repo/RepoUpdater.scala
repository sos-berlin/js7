package js7.controller.repo

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.FileBased
import monix.eval.Task

trait RepoUpdater
{
  def fileBasedVerifier: FileBasedVerifier[FileBased]

  def updateRepo(verifiedUpdateRepo: VerifiedUpdateRepo): Task[Checked[Completed]]
}
