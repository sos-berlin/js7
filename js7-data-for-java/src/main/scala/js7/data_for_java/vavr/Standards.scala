package js7.data_for_java.vavr

import js7.base.problem.Problem

object Standards:
  type VEither[L, R] = io.vavr.control.Either[L, R]
  type VChecked[A] = VEither[Problem, A]
