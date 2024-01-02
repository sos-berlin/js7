package js7.data_for_java.agent;

import js7.data.agent.AgentPath;
import js7.data.subagent.SubagentId;
import static java.util.Arrays.asList;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Test JAgentRef, the Java wrapper for AgentRef.
 */
class JAgentRefTester
{
    private static final String agentRefJson =
       "{\n" +
       "  \"TYPE\": \"AgentRef\",\n" +
       "  \"path\": \"AGENT\",\n" +
       "  \"directors\": [ \"SUBAGENT\" ]\n" +
       "}";

    private final JAgentRef agentRef;

    JAgentRefTester(JAgentRef agentRef) {
        this.agentRef = agentRef;
    }

    void test() {
        testAgentPath();
        testJson();
        assertThat(agentRef.directors(), equalTo(asList(SubagentId.of("SUBAGENT"))));
    }

    private void testAgentPath() {
        assertThat(agentRef.path(), equalTo(AgentPath.of("AGENT")));
    }

    private void testJson() {
        assertThat(getOrThrow(JAgentRef.fromJson(agentRefJson)), equalTo(agentRef));
        assertThat(getOrThrow(JAgentRef.fromJson(agentRef.toJson())), equalTo(agentRef));
    }
}
