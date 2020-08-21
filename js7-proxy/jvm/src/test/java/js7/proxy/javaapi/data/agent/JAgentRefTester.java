package js7.proxy.javaapi.data.agent;

import js7.base.web.Uri;
import js7.data.agent.AgentRefPath;
import js7.data.item.VersionId;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrow;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Test JAgentRef, the Java wrapper for AgentRef.
 */
class JAgentRefTester
{
    private static final JAgentRefId expectedJAgentRefId = JAgentRefId.of("/AGENTREF", "1.0");
    private static final String agentRefJson =
       "{\n" +
       "  \"TYPE\": \"AgentRef\",\n" +
       "  \"path\": \"/AGENTREF\",\n" +
       "  \"versionId\": \"1.0\",\n" +
       "  \"uri\": \"https://agent.example.com\"\n" +
       "}";

    private final JAgentRef agentRef;

    JAgentRefTester(JAgentRef agentRef) {
        this.agentRef = agentRef;
    }

    void test() {
        testAgentRefId();
        testJson();
        assertThat(agentRef.uri(), equalTo(Uri.of("https://agent.example.com")));
        assertThat(agentRef.uri().string(), equalTo("https://agent.example.com"));
    }

    private void testAgentRefId() {
        assertThat(agentRef.id(), equalTo(expectedJAgentRefId));

        VersionId versionId = agentRef.id().versionId();
        assertThat(versionId, equalTo(VersionId.of("1.0")));

        AgentRefPath workflowPath = agentRef.id().path();
        assertThat(workflowPath, equalTo(AgentRefPath.of("/AGENTREF")));
    }

    private void testJson() {
        assertThat(getOrThrow(JAgentRef.fromJson(agentRefJson)), equalTo(agentRef));
        assertThat(getOrThrow(JAgentRef.fromJson(agentRef.toJson())), equalTo(agentRef));
    }
}
