package js7.master.data.javaapi;

import js7.base.problem.JavaChecked;
import js7.base.problem.Problem;
import java.util.Optional;

/**
 * @author Joacim Zschimmer
 */
final class MasterScriptConverterTester
{
    private static final MasterScriptConverter MasterScriptConverter = new MasterScriptConverter();
    private static final String WorkflowScript =
        "define workflow {\n" +
        "  execute agent=\"/AGENT\", executable=\"/EXECUTABLE\";\n" +
        "}\n";
    private static final String WorkflowJson =
        "{" +
            "\"instructions\":[" +
                "{" +
                    "\"TYPE\":\"Execute.Anonymous\"," +
                    "\"job\":{" +
                        "\"agentRefPath\":\"/AGENT\"," +
                        "\"executable\":{" +
                            "\"TYPE\":\"ExecutablePath\"," +
                            "\"path\":\"/EXECUTABLE\"" +
                        "}," +
                        "\"taskLimit\":1" +
                    "}," +
                    "\"sourcePos\":[20,68]" +
                "}," +
                "{\"TYPE\":\"ImplicitEnd\",\"sourcePos\":[70,71]}" +
            "]," +
            "\"source\":\"define workflow {\\n  execute agent=\\\"/AGENT\\\", executable=\\\"/EXECUTABLE\\\";\\n}\\n\"" +
        "}";

    private MasterScriptConverterTester() {}

    static void testWorkflowJsonToScript() {
        JavaChecked<String> checkedScript = MasterScriptConverter.workflowJsonToScript(WorkflowJson);
        assert checkedScript.isValid(): checkedScript.checked();
        assert !checkedScript.problem().isPresent();
        assertEqual(checkedScript.get(), WorkflowScript);
    }

    static void testWorkflowScriptToJson() {
        JavaChecked<String> checkedJson = MasterScriptConverter.workflowScriptToJson(WorkflowScript);
        assert checkedJson.isValid() : checkedJson;
        assert !checkedJson.problem().isPresent();
        assertEqual(checkedJson.get(), WorkflowJson);
    }

    static void testWorkflowScriptSyntaxError() {
        JavaChecked<String> checkedJson = MasterScriptConverter.workflowScriptToJson("ERROR");
        assert checkedJson.isInvalid();
        Optional<Problem> optionalProblem = checkedJson.problem();
        assert optionalProblem.isPresent();
        assertEqual(optionalProblem.get().toString(), "Expected \"define\":1:1, found \"ERROR\"");
    }

    private static void assertEqual(String string, String expected) {
        assert string.equals(expected) : string + " did not equal "/*IntelliJ string*/ + expected;
    }
}
