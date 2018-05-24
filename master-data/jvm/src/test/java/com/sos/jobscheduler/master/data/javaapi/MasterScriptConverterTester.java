package com.sos.jobscheduler.master.data.javaapi;

import com.sos.jobscheduler.base.problem.JavaChecked;
import com.sos.jobscheduler.base.problem.Problem;
import java.util.Optional;

/**
 * @author Joacim Zschimmer
 */
final class MasterScriptConverterTester
{
    private static final MasterScriptConverter MasterScriptConverter = new MasterScriptConverter();
    private static final String WorkflowScript = "job \"/JOB\" on \"/AGENT\";\n";
    private static final String WorkflowJson = "{" +
            "\"instructions\":[{\"TYPE\":\"Job\",\"jobPath\":\"/JOB\",\"agentPath\":\"/AGENT\"}]," +
            "\"source\":\"job \\\"/JOB\\\" on \\\"/AGENT\\\";\\n\"" +
        "}";

    private MasterScriptConverterTester() {}

    static void testWorkflowJsonToScript() {
        JavaChecked<String> checkedScript = MasterScriptConverter.workflowJsonToScript(WorkflowJson);
        assert checkedScript.isValid();
        assert !checkedScript.problem().isPresent();
        assertEqual(checkedScript.get(), WorkflowScript);
    }

    static void testWorkflowScriptToJson() {
        JavaChecked<String> checkedJson = MasterScriptConverter.workflowScriptToJson(WorkflowScript);
        assert checkedJson.isValid();
        assert !checkedJson.problem().isPresent();
        assertEqual(checkedJson.get(), WorkflowJson);
    }

    static void testWorkflowScriptSyntaxError() {
        JavaChecked<String> checkedJson = MasterScriptConverter.workflowScriptToJson("ERROR");
        assert checkedJson.isInvalid();
        Optional<Problem> optionalProblem = checkedJson.problem();
        assert optionalProblem.isPresent();
        assertEqual(optionalProblem.get().toString(), "End:1:1 ...\"ERROR\"");
    }

    private static void assertEqual(String string, String expected) {
        assert string.equals(expected) : string + " did not equal "/*IntelliJ string*/ + expected;
    }
}
