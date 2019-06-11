package com.sos.jobscheduler.base.problem

object Problems
{
  final case class InvalidNameProblem(typeName: String, name: String) extends Problem.Coded {
    def arguments = Map(
      "type" -> typeName,
      "name" -> name
    )
  }

  final case class UnknownKeyProblem(typ: String, key: String) extends Problem.Coded {
    def arguments = Map("type" -> typ, "key" -> key)
  }

  case object InvalidSessionTokenProblem extends Problem.ArgumentlessCoded
}
