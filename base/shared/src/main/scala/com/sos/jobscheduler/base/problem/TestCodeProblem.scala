package com.sos.jobscheduler.base.problem

final case class TestCodeProblem(arguments: Map[String, String]) extends Problem.Coded

object TestCodeProblem extends Problem.Coded.Companion
