package js7.controller.data.javaapi;

import java.util.Optional;
import js7.base.problem.Problem;

/**
 * @author Joacim Zschimmer
 */
final class ControllerJsonValidatorTester
{
    private static final ControllerJsonValidator ControllerJsonValidator = new ControllerJsonValidator();

    private ControllerJsonValidatorTester() {}

    static void testValidWorkflow() {
        Optional<Problem> maybeProblem = ControllerJsonValidator.checkWorkflowJson(
            "{" +
                "\"instructions\": [" +
                    "{ " +
                        "\"TYPE\": \"Execute.Anonymous\"," +
                        "\"job\": { " +
                            "\"agentRefPath\": \"/AGENT\"," +
                            "\"executable\": {" +
                                "\"TYPE\": \"ExecutablePath\"," +
                                "\"path\": \"/JOB\"" +
                            "}, " +
                            "\"taskLimit\": 1" +
                        "}" +
                    "}" +
                "]" +
            "}");
        assert maybeProblem.equals(Optional.empty()) : maybeProblem.toString();
    }

    static void testInvalidWorkflow() {
        Optional<Problem> maybeProblem = ControllerJsonValidator.checkWorkflowJson("{" +
            "\"instructions\": 999" +
          "}");
        assertEqual(maybeProblem.get().toString(), "JSON DecodingFailure at .instructions: C[A]");
    }

    static void testInvalidJson() {
        Optional<Problem> maybeProblem = ControllerJsonValidator.checkWorkflowJson("NO-JSON");
        assertEqual(maybeProblem.get().toString(), "JSON ParsingFailure: expected json value got 'NO-JSO...' (line 1, column 1)");
    }

    static void testValidInstruction() {
        Optional<Problem> maybeProblem = ControllerJsonValidator.checkInstructionJson(
            "{" +
                "\"TYPE\": \"Execute.Anonymous\"," +
                "\"job\": {" +
                    "\"agentRefPath\": \"/AGENT\"," +
                    "\"executable\": {" +
                        "\"TYPE\": \"ExecutablePath\"," +
                        "\"path\": \"/EXECUTABLE\"" +
                    "}," +
                    "\"taskLimit\": 1" +
                "}" +
            "}");
        assert maybeProblem.equals(Optional.empty()) : maybeProblem.toString();
    }

    static void testInvalidInstruction() {
        Optional<Problem> maybeProblem = ControllerJsonValidator.checkInstructionJson(
            "{" +
                "\"TYPE\": \"INVALID\"" +
            "}");
        assertEqual(maybeProblem.get().toString(),
            "JSON DecodingFailure at : Unexpected JSON {\"TYPE\": \"INVALID\", ...} for class 'Instruction'");
    }

    private static void assertEqual(String string, String expected) {
        assert string.equals(expected) : string + " did not equal "/*IntelliJ string*/ + expected;
    }
}
