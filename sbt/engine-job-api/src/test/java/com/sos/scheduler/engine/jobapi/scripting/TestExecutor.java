package com.sos.scheduler.engine.jobapi.scripting;

class TestExecutor {
    private final JobScriptInstanceAdapter adapter;

    TestExecutor(JobScriptInstanceAdapter adapter) {
        this.adapter = adapter;
    }

    final void runLimited(int stepLimit) throws Exception {
        boolean initOk = adapter.callInit(true);
        try {
            if (initOk) {
                try {
                    boolean openOk = adapter.callOpen(true);
                    if (openOk) {
                        int i = 0;
                        while(true) {
                            boolean processOk = adapter.callProcess(false);
                            if (!processOk) break;
                            if (i == stepLimit)  throw new RuntimeException("Step limit "+i+" reached");
                            i++;
                        }
                    }
                } finally {
                    adapter.callClose();
                }
            }
        } finally {
            adapter.callExit();
        }
    }
}
