package js7.data_for_java.order;

import java.util.HashMap;
import java.util.Map;
import js7.data.value.BooleanValue;
import js7.data.value.ListValue;
import js7.data.value.NumberValue;
import js7.data.value.StringValue;
import js7.data.value.Value;
import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

class JOutcomeTester
{
    private JOutcomeTester() {}

    static void testSucceeded(JOutcome outcome) {
        assertThat(outcome, equalTo(JOutcome.succeeded()));
    }

    static void testSucceededRC1(JOutcome outcome) {
        Map<String,Value> namedValues = new HashMap<>();
        namedValues.put("returnCode", NumberValue.of(1));
        namedValues.put("aString", StringValue.of("STRING"));
        namedValues.put("aBoolean", BooleanValue.of(true));
        namedValues.put("aList", ListValue.of(asList(
            NumberValue.of(1), StringValue.of("STRING"), BooleanValue.of(true))));
        assertThat(outcome, equalTo(JOutcome.succeeded(namedValues)));

        assertThat(outcome instanceof JOutcome.Succeeded, equalTo(true));
        JOutcome.Succeeded succeeded = (JOutcome.Succeeded)outcome;

        assertThat(succeeded.namedValues(), equalTo(namedValues));

        {
            Value value = succeeded.namedValues().get("returnCode");
            assertThat(value, equalTo(NumberValue.of(1)));
            assertThat(value instanceof NumberValue, equalTo(true));
            NumberValue numberValue = (NumberValue)value;
            java.math.BigDecimal returnCode = numberValue.number()/*Scala*/.bigDecimal()/*Java*/;
            assertThat(returnCode.intValue(), equalTo(1));
        }
        {
            Value value = succeeded.namedValues().get("aString");
            assertThat(value, equalTo(StringValue.of("STRING")));
            assertThat(value instanceof StringValue, equalTo(true));
            StringValue stringValue = (StringValue)value;
            String aString = stringValue.string();
            assertThat(aString, equalTo("STRING"));
        }
        {
            Value value = succeeded.namedValues().get("aBoolean");
            assertThat(value, equalTo(BooleanValue.of(true)));
            assertThat(value instanceof BooleanValue, equalTo(true));
            BooleanValue booleanValue = (BooleanValue)value;
            Boolean aBoolean = booleanValue.booleanValue();
            assertThat(aBoolean, equalTo(true));
        }
        {
            Value value = succeeded.namedValues().get("aList");
            assertThat(value, equalTo(ListValue.of(asList(
                        NumberValue.of(1), StringValue.of("STRING"), BooleanValue.of(true)))));
            assertThat(value instanceof ListValue, equalTo(true));
            ListValue listValue = (ListValue)value;
            // TODO Provide a JValue wrapper for Value classes ?
            //List<Value> aList = asJava(listValue.list());
            //assertThat(aList, equalTo(true));
        }
    }
}
