package js7.base.problem

final case class TestCodeProblem(arguments: Map[String, String] = Map.empty) extends Problem.Coded


object TestCodeProblem extends Problem.ArgumentlessCoded
