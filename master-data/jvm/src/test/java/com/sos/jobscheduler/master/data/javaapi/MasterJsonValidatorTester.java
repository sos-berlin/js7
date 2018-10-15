package com.sos.jobscheduler.master.data.javaapi;

import com.sos.jobscheduler.base.problem.Problem;
import java.util.Optional;

/**
 * @author Joacim Zschimmer
 */
final class MasterJsonValidatorTester {

    private static final MasterJsonValidator MasterJsonValidator = new MasterJsonValidator();

    private MasterJsonValidatorTester() {}

    static void testValidWorkflow() {
        Optional<Problem> maybeProblem = MasterJsonValidator.checkWorkflowJson(
            "{" +
                "\"instructions\": [" +
                    "{ \"TYPE\": \"Job\", \"jobPath\": \"/JOB\", \"agentPath\": \"/AGENT\" }" +
                "]" +
            "}");
        assert maybeProblem.equals(Optional.empty());
    }

    static void testInvalidWorkflow() {
        Optional<Problem> maybeProblem = MasterJsonValidator.checkWorkflowJson("{" +
            "\"instructions\": 999" +
          "}");
        assertEqual(maybeProblem.get().toString(), "C[A]: DownField(instructions)");
    }

    static void testInvalidJson() {
        Optional<Problem> maybeProblem = MasterJsonValidator.checkWorkflowJson("NO-JSON");
        assertEqual(maybeProblem.get().toString(), "expected json value got N (line 1, column 1)");
    }

    static void testValidInstruction() {
        Optional<Problem> maybeProblem = MasterJsonValidator.checkInstructionJson(
            "{" +
                "\"TYPE\": \"Job\"," +
                "\"jobPath\": \"/JOB\"," +
                "\"agentPath\": \"/AGENT\"" +
            "}");
        assert maybeProblem.equals(Optional.empty());
    }

    static void testInvalidInstruction() {
        Optional<Problem> maybeProblem = MasterJsonValidator.checkInstructionJson(
            "{" +
                "\"TYPE\": \"INVALID\"" +
            "}");
        assertEqual(maybeProblem.get().toString(),
            "com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec$UnknownJsonTypeException: Unexpected JSON {\"TYPE\": \"INVALID\"} for class 'Instruction'");
    }

    private static void assertEqual(String string, String expected) {
        assert string.equals(expected) : string + " did not equal "/*IntelliJ string*/ + expected;
    }
}
