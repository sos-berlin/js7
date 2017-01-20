namespace sos.spooler
{
    using System;

    public abstract class ScriptAdapter
    {
        public Log spooler_log { get; private set; }
        public Task spooler_task { get; private set; }
        public Job spooler_job { get; private set; }
        public Spooler spooler { get; private set; }

        protected string Script { get; private set; }
        protected bool IsOrderJob { get; private set; }

        #region Constructor

        protected ScriptAdapter(
            Log contextLog, Task contextTask, Job contextJob, Spooler contextSpooler, String scriptContent)
        {
            this.spooler_log = contextLog;
            this.spooler_task = contextTask;
            this.spooler_job = contextJob;
            this.spooler = contextSpooler;

            this.Script = scriptContent;
            this.IsOrderJob = this.spooler_job.order_queue() != null;
        }

        #endregion

        #region Public JobScheduler API methods

        #region Public Job_impl methods

        public abstract bool spooler_init();
        public abstract bool spooler_open();
        public abstract bool spooler_process();
        public abstract void spooler_close();
        public abstract void spooler_on_success();
        public abstract void spooler_on_error();
        public abstract void spooler_exit();

        #endregion

        #region Public Monitor_impl methods

        public abstract bool spooler_task_before();
        public abstract bool spooler_process_before();
        public abstract bool spooler_process_after(bool spoolerProcessResult);
        public abstract void spooler_task_after();

        #endregion

        #endregion

        #region Public methods

        public bool ToBoolean(string value)
        {
            return Boolean.Parse(value);
        }

        #endregion
    }
}
