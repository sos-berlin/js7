function spooler_process(){
    spooler_log.info("Hello world");
    var variables = spooler.create_variable_set();
    var taskParams = spooler_task.params;
    variables.merge(taskParams);

    spooler_job.set_delay_after_error(1, 60);
    spooler_job.set_delay_after_error(2, "00:02");
    spooler_job.set_delay_after_error(3, "00:02:22");
    spooler_job.set_delay_after_error(4, "STOP");
    spooler_job.clear_delay_after_error();
    return false;
}
