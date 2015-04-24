package com.sos.scheduler.engine.agent.tests.api;

import sos.spooler.Job_impl;

/**
 * Created by Andreas Liebert on 23.04.2015.
 */
public class JobObjectJob extends Job_impl{
    @Override
    public boolean spooler_process() throws Exception {
        spooler_job.set_delay_after_error(1,60);
        spooler_job.set_delay_after_error(2,"00:02");
        spooler_job.set_delay_after_error(3,"00:02:22");
        spooler_job.set_delay_after_error(4,"STOP");
        spooler_job.clear_delay_after_error();

        return (spooler_task.order() != null);
    }
}
