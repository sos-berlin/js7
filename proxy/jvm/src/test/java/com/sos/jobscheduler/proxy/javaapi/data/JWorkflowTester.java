package com.sos.jobscheduler.proxy.javaapi.data;

import com.sos.jobscheduler.data.filebased.VersionId;
import com.sos.jobscheduler.data.workflow.WorkflowPath;
import com.sos.jobscheduler.data.workflow.WorkflowPrinter;
import com.sos.jobscheduler.proxy.javaapi.utils.JWorkflowParser;
import static com.sos.jobscheduler.proxy.javaapi.utils.VavrUtils.getOrThrowProblem;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Test JWorkflow, the Java wrapper for Workflow.
 * @author Joacim Zschimmer
 */
class JWorkflowTester
{
    private static final JWorkflowId expectedJWorkflowId = JWorkflowId.of("/A-WORKFLOW", "COMMIT-ID");
    private static final String expectedWorkflowNotation =
        "define workflow {\n" +
        "  execute agent=\"/AGENT\", executable=\"/A-EXECUTABLE\";\n" +
        "}\n";

    private final JWorkflow workflow;

    JWorkflowTester(JWorkflow workflow) {
        this.workflow = workflow;
    }

    void test() {
        testWorkflowId();
        testJson();
        testWorkflowPrinter();
        testWorkflowParser();
    }

    private void testWorkflowId() {
        assertThat(workflow.id(), equalTo(expectedJWorkflowId));

        VersionId versionId = workflow.id().versionId();
        assertThat(versionId, equalTo(VersionId.of("COMMIT-ID")));

        WorkflowPath workflowPath = workflow.id().path();
        assertThat(workflowPath, equalTo(WorkflowPath.of("/A-WORKFLOW")));
    }

    private void testJson() {
        String json = workflow.toJson();
        assertThat(json, startsWith("{"));
        assertThat(json, endsWith("}"));
        assertThat(json, containsString("\"path\":\"/A-WORKFLOW\""));

        JWorkflow decodedOrder = getOrThrowProblem(JWorkflow.fromJson(json));
        assertThat(decodedOrder, equalTo(workflow));

        assertThat(getOrThrowProblem(JWorkflow.fromJson(json)),
            equalTo(workflow));
    }

    private void testWorkflowPrinter() {
        assertThat(WorkflowPrinter.print(workflow.underlying()), equalTo(expectedWorkflowNotation));
    }

    private void testWorkflowParser() {
        JWorkflow parsedWorkflow = JWorkflowParser.parse(expectedJWorkflowId, expectedWorkflowNotation)
            .getOrElseGet(problem -> {
                throw new RuntimeException(problem.throwable());
            });
        assertThat(parsedWorkflow, equalTo(workflow));
    }
}
