namespace sos.spooler
{
    public class Job_impl
    {
        public Log spooler_log { get; set; }
        public Task spooler_task { get; set; }
        public Job spooler_job { get; set; }
        public Spooler spooler { get; set; }

        #region JobScheduler API methods

        public virtual bool spooler_init()
        {
            return true;
        }

        public virtual bool spooler_open()
        {
            return true;
        }

        public virtual bool spooler_process()
        {
            return false;
        }

        public virtual void spooler_close()
        {
        }

        public virtual void spooler_on_success()
        {
        }

        public virtual void spooler_on_error()
        {
        }

        public virtual void spooler_exit()
        {
        }

        #endregion
    }
}
