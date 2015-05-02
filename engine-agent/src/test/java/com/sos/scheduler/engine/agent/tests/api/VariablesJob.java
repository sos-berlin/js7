package com.sos.scheduler.engine.agent.tests.api;

import sos.spooler.Job_impl;
import sos.spooler.Order;
import sos.spooler.Variable_set;

/**
 * @author Andreas Liebert
 */
public final class VariablesJob extends Job_impl {
    @Override
    public boolean spooler_process() throws Exception {

        spooler_log.info("Creating variables set");
        Variable_set variableSet = spooler.create_variable_set();

        spooler_log.info("Merging task params");

        Variable_set taskParams = spooler_task.params();
        spooler_log.info(SchedulerAPIIT.TaskParamsCountPrefix() + taskParams.count());
        String substitutedString = taskParams.substitute(SchedulerAPIIT.VariableSubstitutionString());

        spooler_log.info(substitutedString);
        spooler_log.info("Merging task params 2");
        variableSet.merge(taskParams);

        if (spooler_task.order() != null) {
            Order order = spooler_task.order();
            spooler_log.info("Merging order params");
            variableSet.merge(order.params());
            order.params().set_value(SchedulerAPIIT.OrderVariableSetInJob().name(),
                    SchedulerAPIIT.OrderVariableSetInJob().value());

            String orderVarXml = order.params().xml();
            order.params().set_xml(orderVarXml);
        }

        spooler_log.info("Reading params");
        String orderVal = variableSet.var(SchedulerAPIIT.OrderVariable().name());
        String overriddenVal = variableSet.var(SchedulerAPIIT.OrderParamOverridesJobParam().name());
        spooler_log.info(SchedulerAPIIT.OrderVariable().name()+"="+orderVal);
        spooler_log.info(SchedulerAPIIT.OrderParamOverridesJobParam().name()+"="+overriddenVal);

        return spooler_task.order() != null;
    }
}
