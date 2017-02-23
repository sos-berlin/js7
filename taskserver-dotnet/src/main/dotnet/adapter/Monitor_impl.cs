namespace sos.spooler
{
    using System;

    public class Monitor_impl
    {
        public Log spooler_log { get; set; }
        public Task spooler_task { get; set; }
        public Job spooler_job { get; set; }
        public Spooler spooler { get; set; }

        #region JobScheduler API methods

        public virtual bool spooler_task_before()
        {
            return true;
        }

        public virtual bool spooler_process_before()
        {
            return true;
        }

        public virtual bool spooler_process_after(bool spoolerProcessResult)
        {
            return spoolerProcessResult;
        }

        public virtual void spooler_task_after()
        {
        }

        #endregion

        #region Public methods

        public bool ToBoolean(string value)
        {
            return Boolean.Parse(value);
        }

        #endregion
    }
}
