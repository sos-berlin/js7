package  js7.data_for_java.agent;

import js7.base.web.Uri;
import js7.data.agent.AgentPath;
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
       "  \"id\": \"AGENT\",\n" +
       "  \"uri\": \"https://agent.example.com\"\n" +
       "}";

    private final JAgentRef agentRef;

    JAgentRefTester(JAgentRef agentRef) {
        this.agentRef = agentRef;
    }

    void test() {
        testAgentPath();
        testJson();
        assertThat(agentRef.uri(), equalTo(Uri.of("https://agent.example.com")));
        assertThat(agentRef.uri().string(), equalTo("https://agent.example.com"));
    }

    private void testAgentPath() {
        assertThat(agentRef.path(), equalTo(AgentPath.of("AGENT")));
    }

    private void testJson() {
        assertThat(getOrThrow(JAgentRef.fromJson(agentRefJson)), equalTo(agentRef));
        assertThat(getOrThrow(JAgentRef.fromJson(agentRef.toJson())), equalTo(agentRef));
    }
}
