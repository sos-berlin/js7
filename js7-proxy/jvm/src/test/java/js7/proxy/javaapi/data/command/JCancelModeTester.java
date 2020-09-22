package js7.proxy.javaapi.data.command;

import java.util.Arrays;
import java.util.Optional;
import js7.proxy.javaapi.data.workflow.JWorkflowId;
import js7.proxy.javaapi.data.workflow.position.JPosition;
import js7.proxy.javaapi.data.workflow.position.JWorkflowPosition;
import static js7.proxy.javaapi.data.common.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

class JCancelModeTester
{
    private JCancelModeTester() {}

    static void test() {
       assertThat(JCancelMode.freshOnly().toString(),
           equalTo("FreshOnly"));
       assertThat(JCancelMode.kill().toString(),
           equalTo("FreshOrStarted(Some(Kill(false,None)))"));
       assertThat(JCancelMode.kill(true).toString(),
           equalTo("FreshOrStarted(Some(Kill(true,None)))"));
       assertThat(
           JCancelMode.kill(
               true,
               Optional.of(
                   JWorkflowPosition.of(
                       JWorkflowId.of("/WORKFLOW", "1.0"),
                       getOrThrow(
                           JPosition.fromList(Arrays.asList(0, "Then", 1)))))
               ).toString(),
           equalTo("FreshOrStarted(Some(Kill(true,Some(/WORKFLOW~1.0:0/Then:1))))"));
    }
}
