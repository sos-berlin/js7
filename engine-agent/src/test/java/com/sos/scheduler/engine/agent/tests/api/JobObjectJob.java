package com.sos.scheduler.engine.agent.tests.api;

import com.google.common.base.Strings;
import sos.spooler.Job_impl;

/**
 * @author Andreas Liebert
 */
public class JobObjectJob extends Job_impl {

    public enum UnwantedMessages {
        CONFIG_DIR ("spooler_job.configuration_directory is empty"),
        FOLDER_PATH("spooler_job.folder_path is empty"),
        INCLUDE_PATH("spooler_job.include_path is empty");

        private String message;
        UnwantedMessages(String mes){
            message="##"+mes+"##";
        }

        @Override
        public String toString() {
            return message;
        }
    }

    @Override
    public boolean spooler_process() throws Exception {

        spooler_job.set_delay_after_error(1, 60);
        spooler_job.set_delay_after_error(2, "00:02");
        spooler_job.set_delay_after_error(3, "00:02:22");
        spooler_job.set_delay_after_error(4, "STOP");
        spooler_job.clear_delay_after_error();

        checkNotEmpty(spooler_job.configuration_directory(), UnwantedMessages.CONFIG_DIR);
        checkNotEmpty(spooler_job.folder_path(), UnwantedMessages.FOLDER_PATH);
        checkNotEmpty(spooler_job.include_path(), UnwantedMessages.INCLUDE_PATH);

        return (spooler_task.order() != null);
    }

    private void checkNotEmpty(String text, UnwantedMessages message){
        if (Strings.isNullOrEmpty(text)){
            spooler_log.warn(message.toString());
        }
    }
}
