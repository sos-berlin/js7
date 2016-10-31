// $Id: Spooler.java 5854 2008-08-14 08:22:14Z jz $

package sos.spooler;

/*+ Das allgemeine Scheduler-Objekt.
 */
/**
 *
 * @author Joacim Zschimmer, Zschimmer GmbH
 * @version $Revision: 5854 $
 */

public class Spooler extends Idispatch implements HasBean<SpoolerBean>
{
    private                 Spooler             ( long idispatch )                  { super(idispatch); }

    public Spooler(Invoker invoker) {
        super(invoker);
    }

    /*+ @return Wert der Kommandozeilenoption -id= beim Start des Schedulers */
    @SchedulerGetter
    public String           id                  ()                                  { return (String)       com_call( "<id"                             ); }

    /*+ @return Wert der Kommandozeilenoption -param= beim Start des Schedulers */
    @SchedulerGetter
    public String           param               ()                                  { return (String)       com_call( "<param"                          ); }

    /*+ @return Liefert das Arbeitsverzeichnis (als absoluten Pfad) beim Start des Schedulers.
     * Wenn die Kommandozeilenoption -cd= benutzt worden ist, ist es das damit angegebene Verzeichnis. */
    @SchedulerGetter
    public String           directory           ()                                  { return (String)       com_call( "<directory"                      ); }

    /*+ @return Liefert die Einstellung include_path (Kommandozeilen-Option <code>-include=</code>). */
    @SchedulerGetter
    public String           include_path        ()                                  { return (String)       com_call( "<include_path"                   ); }

    /*+ @return Wert der Kommandozeilenoption -log-dir= beim Start des Schedulers.
      * Das ist das Verzeichnis, in dem der Scheduler die Protokolle und Historiendateien ablegt. */
    @SchedulerGetter
    public String           log_dir             ()                                  { return (String)       com_call( "<log_dir"                        ); }

    /*+ @return Liefert den Hostware-Datenbank-Dateinamen (für Historie usw.) */
    @SchedulerGetter
    public String           db_name             ()                                  { return (String)       com_call( "<db_name"                        ); }

    /*+ @return Liefert true, genau dann wenn der Scheduler als Dienst (in Windows) oder als Daemon (in Unix) läuft. */
    @SchedulerGetter
    public boolean          is_service          ()                                  { return        boolean_com_call( "<is_service"                     ); }

    /*+ @return Liefert den Namen des Rechners, auf dem der Scheduler läuft. */
    @SchedulerGetter
    public String           hostname            ()                                  { return (String)       com_call( "<hostname"                       ); }


    /*+ @return Die scheduler-weiten Variablen als {@link Variable_set} */
    @SchedulerGetter
    public Variable_set     variables           ()                                  { return (Variable_set) com_call( "<variables"                      ); }

    /*+ Setzt eine scheduler-weite Variable. */
    public void         set_var                 ( String name, String value )       {                       com_call( ">var", name, value               ); }

    /*+ @return Liefert den Wert einer scheduler-weiten Variablen. */
    public String           var                 ( String name )                     { return (String)       com_call( "<var", name                      ); }

    /*+ @return Eine neue Variablenmenge ({@link Variable_set}).
     * @see Job#start(Variable_set)
     */
    public Variable_set     create_variable_set ()                                  { return (Variable_set) com_call( "create_variable_set"             ); }


    /*+ Liefert das {@link Log} des Schedulers. Normalerweise sollte man aber die Variable {@link Job_impl#spooler_log} verwenden.
     */
    @SchedulerGetter
    public Log              log                 ()                                  { return (Log)          com_call( "<log"                            ); }

  //public IDispatch        script              ()                                  { return (Idispatch)    com_call( "<script"                         ); }

    /*+ @return Der gewünschte {@link Job} */
    public Job              job                 ( String job_name )                 { return (Job)          com_call( "<job", job_name                  ); }


    /*+ @return Liefert eine neue {@link Job_chain}.
      * Diese Jobkette kann, nachdem sie mit Jobs gefüllt worden ist, mit {@link #add_job_chain(Job_chain)} dem Scheduler hinzugefügt werden. */
    public Job_chain        create_job_chain    ()                                  { return (Job_chain)    com_call( "create_job_chain"                ); }


    public void             add_job_chain       ( Job_chain job_chain )             {                       com_call( "add_job_chain", job_chain        ); }

    /*+ @return Liefert die gewünschte Jobkette ({@link Job_chain}) */
    public Job_chain        job_chain           ( String name )                     { return (Job_chain)    com_call( "<job_chain", name                ); }

    /*+ @return Liefert true genau dann, wenn die Jobkette vorhanden (also mit {@link #add_job_chain(Job_chain)} hingefügt worden) ist. */
    public boolean          job_chain_exists    ( String job_chain )                { return        boolean_com_call( "job_chain_exists", job_chain     ); }


    /*+ @return Erzeugt eine neue {@link Order}.
     * Dieser Auftrag kann mit {@link Job_chain#add_order(Order)} einer Jobkette
     * oder mit {@link Order_queue#add_order(Order)} direkt einem Job übergeben werden. */
    public Order            create_order        ()                                  { return (Order)        com_call( "create_order"                    ); }

    public void             terminate           ()                                  {                       com_call( "terminate"                       ); }
    public void             terminate           ( int timeout_in_seconds )          {                       com_call( "terminate", timeout_in_seconds   ); }
    public void             terminate           ( int timeout_in_seconds, boolean restart, boolean all_schedulers, boolean continue_exclusive_scheduler )
                                                                                    {                       com_call( "terminate", new Integer( timeout_in_seconds ),
                                                                                                                                   new Boolean( restart ),
                                                                                                                                   new Boolean( all_schedulers ),
                                                                                                                                   new Boolean( continue_exclusive_scheduler ) ); }

    public void             terminate_and_restart()                                 {                       com_call( "terminate_and_restart"           ); }
    public void             terminate_and_restart( int timeout_in_seconds )         {                       com_call( "terminate_and_restart", timeout_in_seconds); }

    /*+ Sobald alle Aufträge abgerbeitet sind, beendet der Scheduler alle Jobs (durch Aufruf von {@link Job_impl#spooler_close}) und dann sich selbst.
     * Ein neuer Scheduler mit unveränderten Kommandozeilenparametern wird gestartet. */
    public void             let_run_terminate_and_restart()                         {                       com_call( "let_run_terminate_and_restart"   ); }

    /*+ Bricht den Scheduler augenblicklich ab. Kein Job hat Gelegenheit, darauf zu reagieren. */
    public void             abort_immediately   ()                                  {                       com_call( "abort_immediately"               ); }

    /*+ Bricht den Scheduler augenblicklich ab. Kein Job hat Gelegenheit, darauf zu reagieren.
     * Anschließend wird der Scheduler erneut mit den gleichen Kommandozeilenparametern gestartet*/
    public void             abort_immediately_and_restart()                         {                       com_call( "abort_immediately_and_restart"   ); }

    /*+ Name der Datenbanktabelle für die internen Variablen des Schedulers */
    @SchedulerGetter
    public String           db_variables_table_name  ()                             { return (String)       com_call( "<db_variables_table_name" ); }

    /*+ Name der Datenbanktabelle für die Tasks */
    @SchedulerGetter
    public String           db_tasks_table_name      ()                             { return (String)       com_call( "<db_tasks_table_name" ); }

    /*+ Name der Datenbanktabelle für die Aufträge */
    @SchedulerGetter
    public String           db_orders_table_name     ()                             { return (String)       com_call( "<db_orders_table_name" ); }

    /*+ Name der Datenbanktabelle für die Historie */
    @SchedulerGetter
    public String           db_history_table_name    ()                             { return (String)       com_call( "<db_history_table_name" ); }

    /*+ Name der Datenbanktabelle für die Auftragshistorie */
    @SchedulerGetter
    public String           db_order_history_table_name()                           { return (String)       com_call( "<db_order_history_table_name" ); }

    /*+ Option -ini= (factory.ini) */
    @SchedulerGetter
    public String           ini_path()                                              { return (String)       com_call( "<ini_path" ); }

    /*+ XML-Kommando ausführen */
    public String           execute_xml( String xml )                               { return (String)       com_call( "execute_xml", xml ); }

    @SchedulerGetter
    public int              tcp_port()                                              { return            int_com_call( "<tcp_port" ); }

    @SchedulerGetter
    public int              udp_port()                                              { return            int_com_call( "<udp_port" ); }

    public Xslt_stylesheet  create_xslt_stylesheet()                                { return (Xslt_stylesheet)com_call( "Create_xslt_stylesheet" ); }

    /*+ Dasselbe wie create_xslt_stylesheet().load_file( java.io.File ).
      * Steht nur in Java zur Verfügung.
      */
    public Xslt_stylesheet  create_xslt_stylesheet( java.io.File file )             { Xslt_stylesheet stylesheet = (Xslt_stylesheet)com_call( "Create_xslt_stylesheet" );
                                                                                      stylesheet.load_file( file );
                                                                                      return stylesheet; }

    /*+ Dasselbe wie create_xslt_stylesheet().load_xml( String ).
      * Steht nur in Java zur Verfügung.
      */
    public Xslt_stylesheet  create_xslt_stylesheet( String xml )                    { Xslt_stylesheet stylesheet = (Xslt_stylesheet)com_call( "Create_xslt_stylesheet" );
                                                                                      stylesheet.load_xml( xml );
                                                                                      return stylesheet; }

    @SchedulerGetter
    public Locks            locks()                                                 { return (Locks)        com_call( "<locks" ); }

    @SchedulerGetter
    public Process_classes  process_classes()                                       { return (Process_classes)com_call( "<process_classes" ); }

    @SchedulerGetter
    public Supervisor_client supervisor_client()                                    { return (Supervisor_client)com_call( "<supervisor_client" ); }

    @SchedulerGetter
    public String           configuration_directory()                               { return (String)        com_call( "<configuration_directory" ); }

    public Schedule         schedule( String path )                                 { return (Schedule)      com_call( "<schedule", path ); }

    @SchedulerGetter
    public String uri() {
        return (String)com_call("<uri");
    }

    @Override public SpoolerBean toBean() {
        return new SpoolerBean(this);
    }
}
