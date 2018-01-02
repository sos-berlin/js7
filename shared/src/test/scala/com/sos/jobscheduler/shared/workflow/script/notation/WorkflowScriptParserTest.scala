package com.sos.jobscheduler.shared.workflow.script.notation

import com.sos.jobscheduler.common.time.Stopwatch.{measureTime, measureTimeParallel}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflowScript, TestWorkflowScriptNotation}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, NodeId, WorkflowScript}
import com.sos.jobscheduler.shared.workflow.script.WorkflowScriptToGraph.workflowScriptToGraph
import org.scalatest.FreeSpec
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowScriptParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowScriptNotation) == TestWorkflowScript)
  }

  private val singleJobScript = WorkflowScript(List(
    WorkflowScript.Job(NodeId("A"), AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))))

  "Single statement with implicit NodeId" in {
    val source = """job /A on /AGENT;"""
    assert(parse(source) == singleJobScript.copy(source = Some(source)))
  }

  "Single statement with explicit NodeId" in {
    val source = """"A": job /A on /AGENT;"""
    assert(parse(source) == singleJobScript.copy(source = Some(source)))
  }

  "onError and goto" in {
    val source = """
      job /A on /AGENT;
      ifError "ERROR";
      job /B on /AGENT;
      goto "END";
      "ERROR": job /Error on /AGENT;
      "END": end;"""
    assert(parse(source) == WorkflowScript(
      List(
        WorkflowScript.Job(NodeId("A"), AgentJobPath(AgentPath("/AGENT"), JobPath("/A"))),
        WorkflowScript.IfError(NodeId("ERROR")),
        WorkflowScript.Job(NodeId("B"), AgentJobPath(AgentPath("/AGENT"), JobPath("/B"))),
        WorkflowScript.Goto(NodeId("END")),
        WorkflowScript.Job(NodeId("ERROR"), AgentJobPath(AgentPath("/AGENT"), JobPath("/Error"))),
        WorkflowScript.End(NodeId("END"))),
      Some(source)))
  }

  for (n ← sys.props.get("test.speed") map (_.toInt)) "Speed" - {
    s"Parsing $n processes" in {
      info(measureTime(n, "processes") {
        parse(TestWorkflowScriptNotation)
      }.toString)
    }
    s"Compiling $n processes" in {
      val script = parse(TestWorkflowScriptNotation)
      info(measureTime(n, "processes") {
        workflowScriptToGraph(script)
      }.toString)
    }
    s"Parsing and compiling $n processes" in {
      info(measureTime(n, "processes") {
        workflowScriptToGraph(parse(TestWorkflowScriptNotation))
      }.toString)
    }
    s"Parsing and compiling $n processes, parallel" in {
      info(measureTimeParallel(n, "processes") {
        workflowScriptToGraph(parse(TestWorkflowScriptNotation))
      }.toString)
    }
  }

  "Comments" in {
    val source = """/*comment
        */
        //comment
        /*comment/**/job /***//A/**/on/**//AGENT/**/;/**///comment
      """
    assert(parse(source) == singleJobScript.copy(source = Some(source)))
  }

  private def parse(script: String): WorkflowScript =
    WorkflowScriptParser.parse(script) match {
      case Right(workflowScript) ⇒ workflowScript
      case Left(message) ⇒ throw new AssertionError(message) with NoStackTrace
    }
}
