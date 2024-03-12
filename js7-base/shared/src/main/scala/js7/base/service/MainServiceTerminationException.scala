package js7.base.service

import js7.base.utils.ProgramTermination

trait MainServiceTerminationException extends Exception:
  def termination: ProgramTermination
