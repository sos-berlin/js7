// $Id: Job.java 6009 2009-06-12 12:08:23Z jz $

package sos.spooler;

/*+ Operationen auf einen Job.
 * 
 * Ein Objekt dieser Klasse entspricht einem &lt;job> in der Konfiguration. 
 * Von einem Job können nacheinander oder gleichzeitig Tasks ({@link Task}) laufen.
 * <p>
 * Implementiert wird ein Job mit {@link Job_impl}. 
 * 
 * 
 */
/** 
 * @author Joacim Zschimmer
 * @version $Revision: 6009 $
 */

public class Job extends Idispatch implements HasBean<JobBean>
{
    public Job(Invoker invoker) {
        super(invoker);
    }

    private                 Job                 ( long idispatch )                  { super(idispatch); }
    
    /*+ Erzeugt eine neue Task und reiht sie in die Task-Warteschlange ein.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler.job( "job_a" ).start();
     * </pre>
     * @see Job_impl#spooler
     * @see Spooler#job(String)
     * 
     */
    public Task             start               ()                                  { return (Task)       com_call( "start"                         ); }
    
    
    /*+ Erzeugt eine neue Task mit Parametern und reiht sie in die Task-Warteschlange des Jobs ein.
     * 
     * <p>
     * Die Parameter stehen der Task mit {@link Task#params()} zur Verfügung.
     * <p>
     * Zwei besondere Parameter können angegeben werden:
     * <p>
     * "spooler_task_name": Gibt der Task einen Namen, der in den Statusanzeigen erscheint.
     * <p>
     * "spooler_start_after": Gibt eine Zeit in Sekunden (reelle Zahl) an, nach deren Ablauf die Task zu starten ist. 
     * Dabei wird &lt;run_time> nicht beachtet.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      Variable_set parameters = spooler.create_variable_set();
     *      parameters.set_var( "var1", "wert1" );
     *      parameters.set_var( "var2", "wert2" );
     *      spooler.job( "job_a" ).start( parameters );
     * </pre>
     * 
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *      var parameters = spooler.create_variable_set();
     *      parameters( "var1" ) = "wert1";
     *      parameters( "var2" ) = "wert2";
     *      spooler.job( "job_a" ).start( parameters );
     * </pre>
     * 
     * @see Job_impl#spooler
     * @see Spooler#job(String)
     * @see Spooler#create_variable_set()
     * @see Variable_set#set_var(String,String)
     * 
     */
    
    public Task             start               ( Variable_set variables )          { return (Task)       com_call( "start", variables              ); }

    
    
    /*+ Startet eine Task des Jobs, wenn nicht schon eine läuft und &lt;run_time> dies zulässt.
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler.job( "job_a" ).wake();
     * </pre>
     * @see Job_impl#spooler
     * @see Spooler#job(String)
     * 
     */
    
    
    public void             wake                ()                                  {                     com_call( "wake"                          ); }
    
    
    
    /*+ Lässt eine Task starten, sobald sich ein Verzeichnis ändert.
     * 
     * <p>
     * Wenn keine Task des Jobs läuft und sich das Verzeichnis geändert hat
     * (eine Datei hinzukommt, umbenannt oder entfernt wird), 
     * startet der Scheduler innerhalb der &lt;run_time> eine Task.
     * <p>
     * Um mehrere Verzeichnisse zu überwachen, kann der Aufruf wiederholt werden.
     * Ein Aufruf mit einem bereits angegebenen Verzeichnis erneuert die Überwachung.
     * <p>
     * Der Aufruf kann im Startskript oder in spooler_init() des Jobs codiert werden.
     * Wenn er in spooler_init() ist, muss der Job zu Beginn einmal starten, damit er wirksam wird.
     * Verwenden Sie dazu die Einstellung &lt;run_time once="yes">.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler_job.start_when_directory_changed( "c:/tmp" )
     * </pre>
     * @see Job_impl#spooler_job
     * @see Job_impl#spooler_init()
     */
    
    public void             start_when_directory_changed( String directory_name )                           { com_call( "start_when_directory_changed", directory_name ); }

    /*+ Lässt eine Task starten, sobald sich ein Verzeichnis ändert.
    * @see #start_when_directory_changed( String )
    */
    public void             start_when_directory_changed( java.io.File directory_name )                     { com_call( "start_when_directory_changed", directory_name.toString() ); }

    
    /*+ Lässt eine Task starten, sobald sich ein Verzeichnis ändert, mit Angabe eines Regulären Ausdrucks.
     *
     * <p>
     * Wie {@link #start_when_directory_changed(String)}, mit der Einschränkung, dass nur Dateien beachtet werden,
     * deren Name dem angegebenen Regulären Ausdruck entspricht.

     * <p><br/><b>Beispiel</b>
     * <pre>
     *      // Nur Dateien beachten, deren Name nicht auf "~" endet.
     *      spooler_job.start_when_directory_changed( "c:/tmp", "^.*[^~]$" );
     * </pre>
     */
    
    public void             start_when_directory_changed( String directory_name, String filename_pattern )  { com_call( "start_when_directory_changed", directory_name, filename_pattern ); }

    
    /*+ Lässt eine Task starten, sobald sich ein Verzeichnis ändert.
    * @see #start_when_directory_changed( String, String )
    */
    public void             start_when_directory_changed( java.io.File directory_name, String filename_pattern )  { com_call( "start_when_directory_changed", directory_name.toString(), filename_pattern ); }


    /*+ Nimmt alle Aufrufe von start_when_directory_changed() zurück.
     *
     */
    public void             clear_when_directory_changed()                          {                     com_call( "clear_when_directory_changed"  ); }

    
    
  //public Thread           thread              ()                                  { return (Thread)     com_call( "<thread"                       ); }
    
    
    
    /*+ Dasselbe wie spooler().include_path().
     * 
     * @see Spooler#include_path()
     */
    @SchedulerGetter
    public String           include_path        ()                                  { return (String)     com_call( "<include_path"                 ); }
    
    
    
    /*+ Liefert den Jobnamen.
     *  
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler_log.debug( "Mein Jobname ist " + spooler_job.name() );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *      spooler_log.debug( "Mein Jobname ist " + spooler_job.name );
     * </pre>
     * 
     * @return Der Name des Jobs.

     */
    @SchedulerGetter
    public String           name                ()                                  { return (String)     com_call( "<name"                         ); }
    
    
    
    /*+ Setzt für die Status-Anzeige einen Text (für die HTML-Oberfläche).
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler_job.set_state_text( "Datenblock A wird verarbeitet" );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *      spooler_job.state_text = "Datenblock A wird verarbeitet";
     * </pre>
     * @param line Eine Textzeile
     */
    
    public void         set_state_text          ( String line )                     {                     com_call( ">state_text", line             ); }
    
    
    
    /*+ Liefert den in der Konfiguration eingestellten Titel des Jobs.
     *
     * <p>
     * Aus &lt;job title="...">
     *
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler_log.debug( "title=" + spooler_job.title() );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *      spooler_log.debug( "title=" + spooler_job.title );
     * </pre>
     * 
     */
    @SchedulerGetter
    public String           title               ()                                  { return (String)     com_call( "<title"                        ); }

    
    /*+ Liefert die Auftragswarteschlange des Jobs oder null.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler_log.debug( spooler_job.order_queue().length() + " Aufträge sind in der Warteschlange" );
     * </pre>
     * 
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *      spooler_log.debug( spooler_job.order_queue.length + " Aufträge sind in der Warteschlange" );
     * </pre>
     * @return Die {@link Order_queue} oder null, wenn der Job nicht auftragsgesteuert ist (&lt;job order="no">).
     */
    @SchedulerGetter
    public Order_queue      order_queue         ()                                  { return (Order_queue)com_call( "<order_queue"                  ); }

    
    /*+ Stellt die Fehlertoleranz ein.
     * <p>
     * Für verschiedene Anzahlen aufeinanderfolgender Fehler kann eine Verzögerung eingestellt werden.
     * Der Job wird dann nicht gestoppt, sondern die angegebene Zeit verzögert und erneut gestartet.
     * <p>
     * Der Aufruf kann für verschiedene Anzahlen wiederholt werden.
     * Man wird jeweils eine längere Verzögerung angeben.
     * <p>
     * Beispiel siehe {@link #set_delay_after_error(int,String)}
     * 
     * @param error_steps Anzahl der aufeinanderfolgenden Jobfehler, ab der die Verzögerung gilt
     * @param seconds Verzögerung als reele Zahl
     */
    
    public void         set_delay_after_error   ( int error_steps, double seconds ) {                     com_call( ">delay_after_error", new Integer(error_steps), new Double(seconds)   ); }
    
    
    /*+ Wie {@link #set_delay_after_error(int,double)}, "HH:MM:SS" und "STOP" können angegeben werden.
     * 
     * <p>
     * Normalerweise stoppt der Scheduler einen Job, der einen Fehler liefert. 
     * Mit diesem Aufruf können Sie einstellen, dass der Job nicht gestoppt, sondern eine Zeit verzögert werden soll,
     * bevor er neu startet.
     * <p>
     * Die Verzögerung kann als String "HH:MM:SS" oder "HH:MM:SS" (Stunde, Minute, Sekunde) eingestellt werden.
     * <p>
     * Statt einer Zeit kann auch "STOP" angegeben werden. 
     * Wenn der Job die angegebene Anzahl aufeinanderfolgende Fehler erreicht hat,
     * stoppt der Scheduler den Job.
     * <p>
     * Eine gute Stelle für die Aufrufe ist {@link Job_impl#spooler_init()}.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *      spooler_job.set_delay_after_error(  2,  10 );
     *      spooler_job.set_delay_after_error(  5, "00:01" );
     *      spooler_job.set_delay_after_error( 10, "24:00" );
     *      spooler_job.set_delay_after_error( 20, "STOP" );
     * </pre>
     * Nach einem Fehler wiederholt der Scheduler den Job sofort.<br/>
     * Nach dem zweiten bis zum vierten Fehler verzögert der Scheduler den Job um 10 Sekunden,<br/>
     * nach dem fünften bis zum neunten Fehler um eine Minute,
     * nach dem zehnten bis zum neunzehnten um 24 Stunden,
     * nach dem zwanzigsten aufeinanderfolgenden Fehler schließlich stoppt der Job.
     *  
     * @param error_steps Anzahl der aufeinanderfolgenden Jobfehler, ab der die Verzögerung gilt
     * @param hhmm_ss Zeit im Format "HH:MM" oder "HH:MM:SS".
     */
    
    public void         set_delay_after_error   ( int error_steps, String hhmm_ss ) {                     com_call( ">delay_after_error", error_steps, hhmm_ss   ); }
    
    
    /*+ Nimmt alle Aufrufe von set_delay_after_error() zurück.
     */
    public void             clear_delay_after_error()                               {                     com_call( "clear_delay_after_error"       ); }
    
    
    public void         set_delay_order_after_setback( int setback_count, double seconds ) {              com_call( ">delay_order_after_setback", setback_count, new Double( seconds ) ); }

    public void         set_delay_order_after_setback( int setback_count, String hhmm_ss ) {              com_call( ">delay_order_after_setback", setback_count, hhmm_ss ); }

    public void         set_max_order_setbacks  ( int setback_count )               {                     com_call( ">max_order_setbacks"       , new Integer(setback_count) ); }

    @SchedulerGetter
    public int              max_order_setbacks  ()                                  { return     (Integer)com_call( "<max_order_setbacks"); }

    /*+ Entfernt den Job.
      * Der Job wird gestoppt, d.h. laufende Tasks werden beendet, keine neuen werden gestartet.
      * Sobald keine Task mehr läuft, wird der Job entfernt.
      * Tasks in der Warteschlange werden ignoriert.
      * <p>
      * Wenn keine Task des Jobs läuft, entfernt remove() den Job sofort.
      * <p>
      * Auftragsjobs ( &lt;job order="yes">) können nicht entfernt werden.
     */
    public void             remove              ()                                  {                     com_call( "remove"                        ); }

    @SchedulerGetter
    public Process_class    process_class       ()                                  { return (Process_class)com_call( "<process_class"              ); }

    @SchedulerGetter
    public String           folder_path         ()                                  { return (String)     com_call( "<folder_path"                  ); }

    @SchedulerGetter
    public String           configuration_directory()                               { return (String)     com_call( "<configuration_directory"      ); }

    @Deprecated
    @SchedulerGetter
    public int              setback_max         ()                                  { return ((Integer)   com_call( "<setback_max"     )).intValue(); }

    @SchedulerGetter
    public String           script_code         ()                                  { return (String)     com_call( "<script_code"                  ); }

    @Override public final JobBean toBean() {
        return new JobBean(this);
    }
}
