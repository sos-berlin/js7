package js7.data_for_java.order;

import java.util.HashMap;
import java.util.Optional;
import js7.data.item.VersionId;
import js7.data.order.OrderId;
import js7.data.value.StringValue;
import js7.data.value.Value;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.workflow.JWorkflowId;
import js7.data_for_java.workflow.position.JPosition;
import js7.data_for_java.workflow.position.JWorkflowPosition;
import static java.util.Arrays.asList;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * @author Joacim Zschimmer
 */
public class JOrderTester
{
    private static final String aOrderJson =
       "{" +
       "  \"id\":\"A-ORDER\"," +
       "  \"workflowPosition\": {\n" +
       "     \"workflowId\": {\n" +
       "      \"path\": \"A-WORKFLOW\",\n" +
       "      \"versionId\": \"1.0\"\n" +
       "    },\n" +
       "    \"position\": [ 0 ]\n" +
       "  },\n" +
       "  \"state\": {\n" +
       "    \"TYPE\": \"Fresh\"\n" +
       "  },\n" +
       "  \"arguments\": {},\n" +
       "  \"historicOutcomes\": []\n" +
       "}";
    public static final JOrder aOrder = getOrThrow(JOrder.fromJson(aOrderJson));
    private static final String bOrderJson =
       "{" +
       "  \"id\":\"B-ORDER\"," +
       "  \"workflowPosition\": {\n" +
       "     \"workflowId\": {\n" +
       "      \"path\": \"B-WORKFLOW\",\n" +
       "      \"versionId\": \"2.0\"\n" +
       "    },\n" +
       "    \"position\": [ 0 ]\n" +
       "  },\n" +
       "  \"state\": {\n" +
       "    \"TYPE\": \"Ready\"\n" +
       "  },\n" +
       "  \"arguments\": {\n" +
       "    \"key1\": \"value1\",\n" +
       "    \"key2\": \"value2\"\n" +
       "  },\n" +
       "  \"planId\": [ \"DailyPlan\", \"2025-01-29\" ],\n" +
       "  \"historicOutcomes\": [],\n" +
       "  \"deleteWhenTerminated\": true\n" +
       "}";
    public static final JOrder bOrder = getOrThrow(JOrder.fromJson(bOrderJson));

    private final JOrder order;

    public JOrderTester(JOrder order) {
        this.order = order;
    }

    public void test() {
        testId();
        testWorkflowId();
        testJson();
    }

    private void testId() {
        OrderId orderId = order.id();
        assertThat(orderId, equalTo(OrderId.of("A-ORDER")));
    }

    private void testWorkflowId() {
        JWorkflowId workflowId = order.workflowId();
        assertThat(workflowId, equalTo(JWorkflowId.of("A-WORKFLOW", "1.0")));
    }

    private void testJson() {
        String json = order.toJson();
        assertThat(json, startsWith("{"));
        assertThat(json, endsWith("}"));
        assertThat(json, containsString("\"id\":\"A-ORDER\""));

        JOrder decodedOrder = getOrThrow(JOrder.fromJson(json));
        assertThat(decodedOrder, equalTo(order));

        assertThat(getOrThrow(JOrder.fromJson(aOrderJson)),
            equalTo(order));
    }

    static void testForkedOrder(JOrder order) {
        assertThat(order.id(), equalTo(OrderId.of("ORDER-ID|A")));
        assertThat(order.workflowId(), equalTo(JWorkflowId.of(WorkflowPath.of("WORKFLOW"), VersionId.of("1.0"))));
        assertThat(order.workflowPosition().position(), equalTo(getOrThrow(JPosition.fromList(asList(1, "fork+A", 2)))));
        assertThat(order.workflowPosition().position().toList(), equalTo(asList(1, "fork+A", 2)));
        assertThat(order.workflowPosition().position().toString(), equalTo("1/fork+A:2"));
        assertThat(order.workflowPosition(),
            equalTo(JWorkflowPosition.of(
                JWorkflowId.of(WorkflowPath.of("WORKFLOW"), VersionId.of("1.0")),
                getOrThrow(JPosition.fromList(asList(1, "fork+A", 2))))));
        assertThat(order.arguments(), equalTo(new HashMap<String,Value>() {{
            put("KEY", StringValue.of("VALUE"));
        }}));
        assertThat(order.parent(), equalTo(Optional.of(OrderId.of("ORDER-ID"))));
    }
}
