namespace sos.spooler
{
    using System.Management.Automation;

    public class PowershellSpoolerParams : SpoolerParams
    {
        #region Constructors and Destructors

        public PowershellSpoolerParams(Task task, Spooler spooler, bool isOrderJob, bool isShellMode)
            : base(task, spooler, isOrderJob, isShellMode)
        {
        }

        #endregion

        #region Public Properties

        public PSObject items
        {
            get
            {
                var pso = new PSObject();
                var parameters = this.getAll();
                var names = parameters.names().Split(';');
                foreach (var name in names)
                {
                    pso.Properties.Add(new PSVariableProperty(new PSVariable(name, parameters.var(name))));
                }
                return pso;
            }
        }

        #endregion
    }
}
