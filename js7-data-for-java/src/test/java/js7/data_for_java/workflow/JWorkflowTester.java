package  js7.data_for_java.workflow;

import js7.data.item.VersionId;
import js7.data.workflow.WorkflowPath;
import js7.data.workflow.WorkflowPrinter;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Test JWorkflow, the Java wrapper for Workflow.
 * @author Joacim Zschimmer
 */
public class JWorkflowTester
{
    private static final JWorkflowId expectedJWorkflowId = JWorkflowId.of("A-WORKFLOW", "1.0");
    private static final String expectedWorkflowNotation =
        "define workflow {\n" +
        "  execute agent='AGENT', executable='A-EXECUTABLE';\n" +
        "}\n";

    private final JWorkflow workflow;

    public JWorkflowTester(JWorkflow workflow) {
        this.workflow = workflow;
    }

    public void test() {
        testWorkflowId();
        testJson();
        testJsonWithPositions();
        testWorkflowPrinter();
        testWorkflowParser();
    }

    private void testWorkflowId() {
        assertThat(workflow.id(), equalTo(expectedJWorkflowId));

        VersionId versionId = workflow.id().versionId();
        assertThat(versionId, equalTo(VersionId.of("1.0")));

        WorkflowPath workflowPath = workflow.id().path();
        assertThat(workflowPath, equalTo(WorkflowPath.of("A-WORKFLOW")));
    }

    private void testJson() {
        String json = workflow.toJson();
        assertThat(json, startsWith("{"));
        assertThat(json, endsWith("}"));
        assertThat(json, containsString("\"path\":\"A-WORKFLOW\""));

        JWorkflow decodedOrder = getOrThrow(JWorkflow.fromJson(json));
        assertThat(decodedOrder, equalTo(workflow));

        assertThat(getOrThrow(JWorkflow.fromJson(json)),
            equalTo(workflow));
    }

    private void testJsonWithPositions() {
        String json = workflow.withPositions().toJson();
        assertThat(json, startsWith("{"));
        assertThat(json, endsWith("}"));
        assertThat(json, containsString("\"position\":[0]"));
        assertThat(json, containsString("\"position\":[1]"));
        // If End instruction is missing, ImplicitEnd is added with its (thought) position
        assertThat(json, containsString("\"TYPE\":\"ImplicitEnd\""));
    }

    private void testWorkflowPrinter() {
        assertThat(WorkflowPrinter.print(workflow.asScala()), equalTo(expectedWorkflowNotation));
    }

    private void testWorkflowParser() {
        JWorkflow parsedWorkflow = JWorkflowParser.parse(expectedJWorkflowId, expectedWorkflowNotation)
            .getOrElseGet(problem -> {
                throw new RuntimeException(problem.throwable());
            });
        assertThat(parsedWorkflow, equalTo(workflow));
    }
}
