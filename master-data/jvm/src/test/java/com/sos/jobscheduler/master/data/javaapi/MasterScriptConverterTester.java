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
    private static final String WorkflowScript =
        "workflow {\n" +
        "  execute executable=\"/EXECUTABLE\", agent=\"/AGENT\";\n" +
        "}\n";
    private static final String WorkflowJson =
        "{" +
            "\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"/AGENT\",\"executablePath\":\"/EXECUTABLE\",\"taskLimit\":1}}]," +
            "\"source\":\"workflow {\\n  execute \\\"/EXECUTABLE\\\", agent=\\\"/AGENT\\\";\\n}\\n\"" +
        "}";

    private MasterScriptConverterTester() {}

    static void testWorkflowJsonToScript() {
        JavaChecked<String> checkedScript = MasterScriptConverter.workflowJsonToScript(WorkflowJson);
        assert checkedScript.isValid(): checkedScript.checked().toString();
        assert !checkedScript.problem().isPresent();
        assertEqual(checkedScript.get(), WorkflowScript);
    }

    static void testWorkflowScriptToJson() {
        JavaChecked<String> checkedJson = MasterScriptConverter.workflowScriptToJson(WorkflowScript);
        assert checkedJson.isValid() : checkedJson.toString();
        assert !checkedJson.problem().isPresent();
        assertEqual(checkedJson.get(), WorkflowJson);
    }

    static void testWorkflowScriptSyntaxError() {
        JavaChecked<String> checkedJson = MasterScriptConverter.workflowScriptToJson("ERROR");
        assert checkedJson.isInvalid() ;
        Optional<Problem> optionalProblem = checkedJson.problem();
        assert optionalProblem.isPresent();
        assertEqual(optionalProblem.get().toString(), "\"workflow\":1:1 ...\"ERROR\"");
    }

    private static void assertEqual(String string, String expected) {
        assert string.equals(expected) : string + " did not equal "/*IntelliJ string*/ + expected;
    }
}
