package js7.proxy.javaapi.data.agent;

import js7.base.web.Uri;
import js7.data.agent.AgentName;
import static js7.proxy.javaapi.data.common.VavrUtils.getOrThrow;
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
       "  \"name\": \"AGENT\",\n" +
       "  \"uri\": \"https://agent.example.com\"\n" +
       "}";

    private final JAgentRef agentRef;

    JAgentRefTester(JAgentRef agentRef) {
        this.agentRef = agentRef;
    }

    void test() {
        testAgentName();
        testJson();
        assertThat(agentRef.uri(), equalTo(Uri.of("https://agent.example.com")));
        assertThat(agentRef.uri().string(), equalTo("https://agent.example.com"));
    }

    private void testAgentName() {
        assertThat(agentRef.name(), equalTo(AgentName.of("AGENT")));
    }

    private void testJson() {
        assertThat(getOrThrow(JAgentRef.fromJson(agentRefJson)), equalTo(agentRef));
        assertThat(getOrThrow(JAgentRef.fromJson(agentRef.toJson())), equalTo(agentRef));
    }
}
