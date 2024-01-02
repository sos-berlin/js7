package js7.data_for_java.workflow.position;

import static java.util.Arrays.asList;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

class JPositionTester
{
    private static final String positionJson = "[ 1, \"then\", 2, \"fork+BRANCH\", 3 ]";

    private final JPosition position;

    JPositionTester(JPosition position) {
        this.position = position;
    }

    void test() {
        assertThat(position, equalTo(getOrThrow(JPosition.fromList(asList(1, "then", 2, "fork+BRANCH", 3)))));
        assertThat(position.toString(), equalTo("1/then:2/fork+BRANCH:3"));
        assertThat(getOrThrow(JPosition.fromJson(positionJson)), equalTo(position));
        assertThat(getOrThrow(JPosition.fromJson(position.toJson())), equalTo(position));
    }
}
