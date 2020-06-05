package js7.proxy.javaapi.data;

import js7.data.order.OrderId;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrowProblem;
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
    static final String aOrderJson =
       "{" +
       "  \"id\":\"A-ORDER\"," +
       "  \"workflowPosition\": {\n" +
       "     \"workflowId\": {\n" +
       "      \"path\": \"/A-WORKFLOW\",\n" +
       "      \"versionId\": \"COMMIT-ID\"\n" +
       "    },\n" +
       "    \"position\": [ 0 ]\n" +
       "  },\n" +
       "  \"state\": {\n" +
       "    \"TYPE\": \"Fresh\"\n" +
       "  },\n" +
       "  \"arguments\": {},\n" +
       "  \"historicOutcomes\": []\n" +
       "}";
    static final JOrder aOrder = getOrThrowProblem(JOrder.fromJson(aOrderJson));
    static final String bOrderJson =
       "{" +
       "  \"id\":\"B-ORDER\"," +
       "  \"workflowPosition\": {\n" +
       "     \"workflowId\": {\n" +
       "      \"path\": \"/B-WORKFLOW\",\n" +
       "      \"versionId\": \"COMMIT-ID\"\n" +
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
       "  \"historicOutcomes\": []\n" +
       "}";
    static final JOrder bOrder = getOrThrowProblem(JOrder.fromJson(bOrderJson));

    private final JOrder order;

    JOrderTester(JOrder order) {
        this.order = order;
    }

    void test() {
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
        assertThat(workflowId, equalTo(JWorkflowId.of("/A-WORKFLOW", "COMMIT-ID")));
    }

    private void testJson() {
        String json = order.toJson();
        assertThat(json, startsWith("{"));
        assertThat(json, endsWith("}"));
        assertThat(json, containsString("\"id\":\"A-ORDER\""));

        JOrder decodedOrder = getOrThrowProblem(JOrder.fromJson(json));
        assertThat(decodedOrder, equalTo(order));

        assertThat(getOrThrowProblem(JOrder.fromJson(aOrderJson)),
            equalTo(order));
    }
}
