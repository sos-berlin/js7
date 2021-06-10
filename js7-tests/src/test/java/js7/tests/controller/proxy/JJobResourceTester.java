package js7.tests.controller.proxy;

import java.util.HashMap;
import java.util.concurrent.CompletableFuture;
import js7.base.crypt.SignedString;
import js7.data.event.Event;
import js7.data.item.VersionId;
import js7.data.job.JobResourcePath;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data_for_java.controller.JControllerState;
import js7.data_for_java.jobresource.JJobResource;
import js7.data_for_java.order.JFreshOrder;
import js7.data_for_java.value.JExpression;
import js7.data_for_java.workflow.JWorkflow;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSigned;
import static js7.data_for_java.item.JUpdateItemOperation.addVersion;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

final class JJobResourceTester
{
    private final JControllerApi api;
    private final JControllerProxy proxy;

    JJobResourceTester(JControllerProxy proxy) {
        this.api = proxy.api();
        this.proxy = proxy;
    }

    void run() throws Exception
    {
        JJobResource aJobResource = JJobResource.of(
            JobResourcePath.of("JOB-RESOURCE-A"),
            emptyMap(),
            new HashMap<String, JExpression>() {{
                put("A", getOrThrow(JExpression.parse("'a'")));
                put("B", getOrThrow(JExpression.parse("'bb'")));
            }});
        JJobResource bJobResource = JJobResource.of(
            JobResourcePath.of("JOB-RESOURCE-B"),
            emptyMap(),
            new HashMap<String, JExpression>() {{
                put("B", getOrThrow(JExpression.parse("'IGNORED'")));
                put("C", getOrThrow(JExpression.parse("'ccc'")));
                put("E", getOrThrow(JExpression.parse("'E OF JOB RESOURCE'")));
            }});
        JWorkflow workflow = getOrThrow(JWorkflow.fromJson("{\n" +
            "  \"TYPE\": \"Workflow\",\n" +
            "  \"path\": \"JOB-RESOURCE-WORKFLOW\",\n" +
            "  \"versionId\": \"JJobResourceTester-VERSION\",\n" +
            "  \"instructions\": [\n" +
            "    {\n" +
            "      \"TYPE\": \"Execute.Anonymous\",\n" +
            "      \"job\": {\n" +
            "        \"agentPath\": \"AGENT\",\n" +
            "        \"executable\": {\n" +
            "          \"TYPE\": \"ScriptExecutable\",\n" +
            "          \"script\": \"#!/usr/bin/env bash\\nset -euo pipefail\\necho A=/$A/\\necho B=/$B/\\necho C=/$C/\\necho D=/$D/\\necho E=/$E/\\n\",\n" +
            "          \"env\": {\n" +
            "            \"D\": \"'D OF JOB'\",\n" +
            "            \"E\": \"'E OF JOB'\"\n" +
            "          }\n" +
            "        },\n" +
            "        \"jobResourcePaths\": [\n" +
            "          \"JOB-RESOURCE-A\",\n" +
            "          \"JOB-RESOURCE-B\"\n" +
            "        ]\n" +
            "      }\n" +
            "    }\n" +
            "  ]\n" +
            "}\n"));

        await(
            api.updateItems(Flux.concat(
                Flux.just(addVersion(VersionId.of("JJobResourceTester-VERSION"))),
                Flux.fromIterable(asList(workflow, aJobResource, bJobResource))
                    .map(o -> addOrChangeSigned(sign(JControllerState.inventoryItemToJson(o)))))));

        OrderId orderId = OrderId.of("JOB-RESOURCE");

        CompletableFuture<Void> orderTerminated = new CompletableFuture<>();
        StringBuilder output = new StringBuilder();
        Flux<?> flux = proxy.flux().doOnNext(eventAndState -> {
            if (eventAndState.stampedEvent().value().key().equals(orderId)) {
                Event event = eventAndState.stampedEvent().value().event();
                if (event instanceof OrderEvent.OrderStdWritten) {
                    output.append(((OrderEvent.OrderStdWritten)event).chunk());
                } else if (event instanceof OrderEvent.OrderTerminated) {
                    orderTerminated.complete(null);
                }
            }
        });
        Disposable subscription = flux.subscribe();

        await(
            api.addOrders(Flux.just(
                JFreshOrder.of(orderId, workflow.id().path()))));

        try {
            orderTerminated.get(99, SECONDS);
        } finally {
            subscription.dispose();
        }

        assertThat(output.toString().replaceAll("\r", ""), equalTo(
            "A=/a/\n" +
            "B=/bb/\n" +
            "C=/ccc/\n" +
            "D=/D OF JOB/\n" +
            "E=/E OF JOB RESOURCE/\n"));
    }

    private static SignedString sign(String json) {
        return SignedString.of(
            json,                       // The string to be be signed
            "Silly",                    // Thy signature type, "X509" or "PGP" (or "Silly" for silly testing)
            "MY-SILLY-SIGNATURE");      // The signature of string (in case of X.509: MIME base64 encoded)
    }
}
