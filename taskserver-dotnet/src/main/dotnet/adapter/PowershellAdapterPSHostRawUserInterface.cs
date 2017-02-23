namespace sos.spooler
{
    using System;
    using System.Management.Automation.Host;

    internal class PowershellAdapterPSHostRawUserInterface : PSHostRawUserInterface
    {
        #region Constants and Fields

        private const int DefaultConsoleHeight = 100;
        private const int DefaultConsoleWidth = 150;
        private readonly Size windowSize;
        private Size bufferSize;

        #endregion

        #region Constructors and Destructors

        public PowershellAdapterPSHostRawUserInterface()
        {
            this.ForegroundColor = ConsoleColor.White;
            this.BackgroundColor = ConsoleColor.Black;

            this.bufferSize = new Size(DefaultConsoleWidth, DefaultConsoleHeight);
            this.windowSize = new Size(DefaultConsoleWidth, DefaultConsoleHeight);
        }

        #endregion

        #region Public Properties

        public override sealed ConsoleColor BackgroundColor { get; set; }

        public override Size BufferSize
        {
            get
            {
                return this.bufferSize;
            }
            set
            {
                this.bufferSize = value;
            }
        }

        public override Coordinates CursorPosition { get; set; }

        public override int CursorSize { get; set; }

        public override sealed ConsoleColor ForegroundColor { get; set; }

        public override bool KeyAvailable
        {
            get
            {
                return false;
            }
        }

        public override Size MaxPhysicalWindowSize
        {
            get
            {
                return this.windowSize;
            }
        }

        public override Size MaxWindowSize
        {
            get
            {
                return this.windowSize;
            }
        }

        public override Coordinates WindowPosition { get; set; }

        public override Size WindowSize { get; set; }

        public override string WindowTitle { get; set; }

        #endregion

        #region Public Methods

        public override void FlushInputBuffer()
        {
        }

        public override BufferCell[,] GetBufferContents(Rectangle rectangle)
        {
            throw new NotImplementedException();
        }

        public override KeyInfo ReadKey(ReadKeyOptions options)
        {
            throw new NotImplementedException();
        }

        public override void ScrollBufferContents(
            Rectangle source, Coordinates destination, Rectangle clip, BufferCell fill)
        {
        }

        public override void SetBufferContents(Coordinates origin, BufferCell[,] contents)
        {
        }

        public override void SetBufferContents(Rectangle rectangle, BufferCell fill)
        {
        }

        #endregion
    }
}
