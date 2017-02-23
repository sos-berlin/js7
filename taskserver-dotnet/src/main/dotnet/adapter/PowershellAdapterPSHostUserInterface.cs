namespace sos.spooler
{
    using System;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.Management.Automation;
    using System.Management.Automation.Host;
    using System.Security;

    internal class PowershellAdapterPSHostUserInterface : PSHostUserInterface
    {
        #region Constants and Fields

        private readonly PowershellAdapterPSHostRawUserInterface rawUi;
        private readonly Log spooler_log;

        #endregion

        #region Constructors and Destructors

        public PowershellAdapterPSHostUserInterface(Log log)
        {
            this.spooler_log = log;
            this.rawUi = new PowershellAdapterPSHostRawUserInterface();
        }

        #endregion

        #region Public Properties

        public String LastInfoMessage { get; set; }

        public override PSHostRawUserInterface RawUI
        {
            get
            {
                return this.rawUi;
            }
        }

        #endregion

        #region Public Methods

        public override Dictionary<string, PSObject> Prompt(
            string caption, string message, Collection<FieldDescription> descriptions)
        {
            throw new NotImplementedException();
        }

        public override int PromptForChoice(
            string caption, string message, Collection<ChoiceDescription> choices, int defaultChoice)
        {
            throw new NotImplementedException();
        }

        public override PSCredential PromptForCredential(
            string caption, string message, string userName, string targetName)
        {
            throw new NotImplementedException();
        }

        public override PSCredential PromptForCredential(
            string caption, string message, string userName, string targetName, PSCredentialTypes allowedCredentialTypes,
            PSCredentialUIOptions options)
        {
            throw new NotImplementedException();
        }

        public override string ReadLine()
        {
            throw new NotImplementedException();
        }

        public override SecureString ReadLineAsSecureString()
        {
            throw new NotImplementedException();
        }

        public override void Write(string value)
        {
            this.WriteInfo(value);
        }

        public override void Write(ConsoleColor foregroundColor, ConsoleColor backgroundColor, string value)
        {
            this.WriteInfo(value);
        }

        public override void WriteDebugLine(string message)
        {
            this.spooler_log.debug3(GetOutputMessage(message));
        }

        public override void WriteErrorLine(string message)
        {
            Console.Error.WriteLine(GetOutputMessage(message));
        }

        public override void WriteLine(string value)
        {
            this.WriteInfo(value);
        }

        public override void WriteProgress(long sourceId, ProgressRecord record)
        {
        }

        public override void WriteVerboseLine(string message)
        {
            this.spooler_log.debug(GetOutputMessage(message));
        }

        public override void WriteWarningLine(string message)
        {
            this.spooler_log.warn(GetOutputMessage(message));
        }

        #endregion

        #region Methods

        private static string GetOutputMessage(string msg)
        {
            return msg ?? "";
        }

        private void WriteInfo(string message)
        {
            var val = GetOutputMessage(message);
            this.LastInfoMessage = val;
            this.spooler_log.info(val);
        }

        #endregion
    }
}
