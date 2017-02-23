namespace sos.spooler
{
    using System;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.Linq;
    using System.Management.Automation;
    using System.Management.Automation.Runspaces;
    using System.Text;

    public class PowershellAdapter : ScriptAdapter
    {
        #region Constants and Fields

        private PowershellAdapterPSHost host;
        private bool isShellMode;
        private Runspace runspace;
        private PowershellSpoolerParams spoolerParams;

        #endregion

        #region Constructors and Destructors

        public PowershellAdapter(
            Log contextLog, Task contextTask, Job contextJob, Spooler contextSpooler, String scriptContent)
            : base(contextLog, contextTask, contextJob, contextSpooler, scriptContent)
        {
            this.ParseScript();
            this.spoolerParams = new PowershellSpoolerParams(
                this.spooler_task, this.spooler, this.IsOrderJob, this.isShellMode);

            this.host = new PowershellAdapterPSHost(this.spooler_log);
            this.runspace = RunspaceFactory.CreateRunspace(this.host);
            this.runspace.Open();
            this.runspace.SessionStateProxy.SetVariable("spooler_log", this.spooler_log);
            this.runspace.SessionStateProxy.SetVariable("spooler_task", this.spooler_task);
            this.runspace.SessionStateProxy.SetVariable("spooler_job", this.spooler_job);
            this.runspace.SessionStateProxy.SetVariable("spooler", this.spooler);
            this.runspace.SessionStateProxy.SetVariable("spooler_params", this.spoolerParams);
        }

        #endregion

        #region Public Methods

        public override void spooler_close()
        {
            if (this.isShellMode)
            {
                return;
            }

            try
            {
                this.InvokeFunction("spooler_close");
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override void spooler_exit()
        {
            try
            {
                if (this.isShellMode)
                {
                }
                else
                {
                    this.InvokeFunction("spooler_exit");
                }
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
            finally
            {
                this.Close();
            }
        }

        public override bool spooler_init()
        {
            if (this.isShellMode)
            {
                return true;
            }

            try
            {
                this.InitializeScript(false);
                var result = this.InvokeFunction("spooler_init");
                return GetReturnValue(result, true);
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override void spooler_on_error()
        {
            if (this.isShellMode)
            {
                return;
            }
            try
            {
                this.InvokeFunction("spooler_on_error");
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override void spooler_on_success()
        {
            if (this.isShellMode)
            {
                return;
            }
            try
            {
                this.InvokeFunction("spooler_on_success");
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override bool spooler_open()
        {
            if (this.isShellMode)
            {
                return true;
            }

            try
            {
                var result = this.InvokeFunction("spooler_open");
                return GetReturnValue(result, true);
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override bool spooler_process()
        {
            try
            {
                if (this.isShellMode)
                {
                    this.spoolerParams.SetEnvVars();
                    this.InitializeScript(true);
                    this.CheckLastExitCode(true);
                    return this.IsOrderJob;
                }

                var result = this.InvokeFunction("spooler_process");
                return GetReturnValue(result, this.IsOrderJob);
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override bool spooler_process_after(bool spoolerProcessResult)
        {
            try
            {
                var result = this.InvokeFunction("spooler_process_after", spoolerProcessResult);
                return GetReturnValue(result, spoolerProcessResult);
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override bool spooler_process_before()
        {
            try
            {
                var result = this.InvokeFunction("spooler_process_before");
                return GetReturnValue(result, true);
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        public override void spooler_task_after()
        {
            try
            {
                this.InvokeFunction("spooler_task_after");
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
            finally
            {
                this.Close();
            }
        }

        public override bool spooler_task_before()
        {
            try
            {
                this.InitializeScript(false);
                var result = this.InvokeFunction("spooler_task_before");
                return GetReturnValue(result, true);
            }
            catch (RuntimeException ex)
            {
                throw new Exception(GetErrorMessage(ex.ErrorRecord));
            }
        }

        #endregion

        #region Methods

        private static string GetErrorMessage(ErrorRecord errorRecord)
        {
            var sb = new StringBuilder(errorRecord.ToString());
            sb.Append(errorRecord.InvocationInfo.PositionMessage);
            return sb.ToString();
        }

        private static bool GetReturnValue(string result, bool defaultValue)
        {
            var rs = defaultValue;
            if (result != null)
            {
                try
                {
                    rs = Boolean.Parse(result);
                }
                catch (Exception)
                {
                }
            }
            return rs;
        }

        private void CheckLastExitCode(bool useLocalScope)
        {
            var lastExitCode = this.InvokeCommand("$Global:LastExitCode", useLocalScope).FirstOrDefault();
            var exitCode = 0;
            if (lastExitCode != null)
            {
                try
                {
                    exitCode = Int32.Parse(lastExitCode.ToString());
                }
                catch (Exception)
                {
                }
            }

            if (exitCode == 0)
            {
                return;
            }
            this.spooler_log.error(
                String.Format(
                    "Process terminated with exit code {0}. See the following warning SCHEDULER-280.", exitCode));
            this.spooler_task.set_exit_code(exitCode);
        }

        private void Close()
        {
            this.runspace.Close();
            this.runspace.Dispose();

            this.runspace = null;
            this.host = null;
            this.spoolerParams = null;
        }

        private void InitializeScript(bool useLocalScope)
        {
            this.InvokeScript(this.Script, useLocalScope);
        }

        private IEnumerable<PSObject> InvokeCommand(String command, bool useLocalScope)
        {
            Collection<PSObject> result;
            using (var pipeline = this.runspace.CreatePipeline())
            {
                pipeline.Commands.AddScript(command, useLocalScope);
                result = pipeline.Invoke();
            }
            return result;
        }

        private string InvokeFunction(String functionName, bool? param = null)
        {
            var functionParams = "";
            if (param.HasValue)
            {
                functionParams = "($" + param.Value + ")";
            }

            var command = String.Format(
                "if($function:{0}){{ {1}{2} }}",
                functionName,
                functionName,
                functionParams);

            return this.InvokeScript(command, false);
        }

        private string InvokeScript(String command, bool useLocalScope)
        {
            using (var pipeline = this.runspace.CreatePipeline())
            {
                pipeline.Commands.AddScript(command, useLocalScope);
                pipeline.Commands.Add("Out-Default");
                pipeline.Commands[0].MergeMyResults(PipelineResultTypes.Error, PipelineResultTypes.Output);
                pipeline.Invoke();
            }
            return ((PowershellAdapterPSHostUserInterface)this.host.UI).LastInfoMessage;
        }

        private void ParseScript()
        {
            if (string.IsNullOrEmpty(this.Script))
            {
                throw new Exception("Script is null or empty.");
            }

            Collection<PSParseError> parseErrors;
            var tokens = PSParser.Tokenize(this.Script, out parseErrors);
            var apiFunction =
                tokens.FirstOrDefault(
                    t => t.Type.Equals(PSTokenType.CommandArgument) &&
                         (t.Content.Equals("spooler_init")
                          || t.Content.Equals("spooler_open")
                          || t.Content.Equals("spooler_process")
                          || t.Content.Equals("spooler_close")
                          || t.Content.Equals("spooler_on_success")
                          || t.Content.Equals("spooler_on_error")
                          || t.Content.Equals("spooler_exit")));
            this.isShellMode = apiFunction == null;
        }

        #endregion
    }
}
