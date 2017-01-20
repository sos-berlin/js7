package com.sos.scheduler.engine.taskserver.dotnet;

import com.sos.scheduler.engine.taskserver.dotnet.api.TaskContext;

public final class DotnetJob extends sos.spooler.Job_impl {
    private final DotnetApiImpl apiImpl;

    DotnetJob(TaskContext taskContext, DotnetApiImpl dotnetApiImpl)
            throws Exception {
        spooler_log = taskContext.spoolerLog();
        spooler_task = taskContext.spoolerTask();
        spooler_job = taskContext.spoolerJob();
        spooler = taskContext.spooler();
        this.apiImpl = dotnetApiImpl;
    }

    @Override
    public boolean spooler_init() throws Exception {
        return apiImpl.spooler_init();
    }

    @Override
    public void spooler_exit() throws Exception {
        apiImpl.spooler_exit();
    }

    @Override
    public boolean spooler_open() throws Exception {
        return apiImpl.spooler_open();
    }

    @Override
    public void spooler_close() throws Exception {
        apiImpl.spooler_close();
    }

    @Override
    public boolean spooler_process() throws Exception {
        return apiImpl.spooler_process();
    }

    @Override
    public void spooler_on_error() throws Exception {
        apiImpl.spooler_on_error();
    }

    @Override
    public void spooler_on_success() throws Exception {
        apiImpl.spooler_on_success();
    }
}
