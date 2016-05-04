// $Id: Job_impl.java 4607 2006-11-30 09:10:11Z jz $

package sos.spooler;

/*+
 * Oberklasse für die Implementierung eines Startskripts oder eines Jobs.
 * 
 * Der Methoden eines Jobs werden in folgender Reihenfolge aufgerufen.
 * <pre>
 *     spooler_init()
 *         spooler_open()
 *             spooler_process()
 *             spooler_process()
 *             ...
 *         spooler_close()
 *         spooler_on_success() oder spooler_on_error()
 *     spooler_exit()
 * </pre>
 * 
 * <p>
 *      Keine dieser Methoden muss implementiert werden. In der Regel wird wenigstens spooler_process() implementiert.
 * </p>
 * <p>
 *  Ein Fehler beim Ausführen des Job-Skripts während des Ladens oder in spooler_init() führt zum Aufruf von spooler_on_error(). Der Job wird gestoppt. spooler_exit() wird gerufen (obwohl spooler_init() nicht gerufen worden ist!) und die Scripting Engine wird geschlossen.
 * </p>
 * <p>
 *  spooler_on_error() muss also auch mit Fehlern umgehen, die beim Laden oder in spooler_init() auftreten. 
 * </p>
 * <p>
 *  spooler_exit() wird gerufen, auch wenn spooler_init() nicht gerufen worden ist.
 * </p>
 */
/** 
 * @author Joacim Zschimmer, Zschimmer GmbH
 * @version $Revision: 4607 $
 */

public class Job_impl
{
    protected Job_impl()
    {
    }
    
    
    /*+ Der Scheduler ruft diese Methode nach dem Konstruktor und vor {@link #spooler_open()} genau einmal auf.
      * Gegenstück ist {@link #spooler_exit()}. Die Methode ist geeignet, um die Task zu initialisieren 
      * (z.B. um eine Datenbank-Verbindung aufzubauen).
      * 
      * @return false beendet die Task. Der Scheduler setzt mit spooler_exit() fort.
      * Wenn die Task einen Auftrag verarbeiten soll, wird die Task mit Fehler beendet,
      * was den Job stoppt (wenn nicht {@link Job#set_delay_after_error(int,double)} gesetzt ist).
      */
    
    public boolean  spooler_init        ()      throws Exception  { return true; }

    

    /*+ Wir als allerletzte Methode gerufen, bevor das Java-Objekt verworfen wird. 
      * Hier kann z.B. eine Datenbank-Verbindung geschlossen werden. 
      */
    
    public void     spooler_exit        ()      throws Exception  {}


    
    /*+ Wird zu Beginn einer Task gerufen. 
      * Die Methode wird direkt nach {@link #spooler_init()} gerufen, es gibt derzeit keinen Unterschied.
      * Gegenstück ist {@link #spooler_close()}.
      * @return false beendet die Task. Der Scheduler setzt mit spooler_close() fort.
      */

    public boolean  spooler_open        ()      throws Exception  { return true; }


    
    /*+ Wird am Ende eines Joblaufs gerufen.
      * Gegenstück zu {@link #spooler_open()}.
      */

    public void     spooler_close       ()      throws Exception  {}


    /*+ Führt einen Jobschritt aus.
      * Gegenstück ist {@link #spooler_exit()}.
      * Die Default-Implementierung gibt false zurück (versetzt aber einen Auftrag in den Folgezustand).
      * 
      * @return bei &lt;job order="no">: false beendet den Joblauf.
      * Bei &lt;job order="true">: false versetzt den Auftrag in den Fehlerzustand (s. {@link Job_chain_node}).
      */

    public boolean  spooler_process     ()      throws Exception  
    { 
        //spooler_method_not_implemented(); 
        return false; 
    }


    /*+ Wird als letzte Funktion eines Joblaufs gerufen, wenn ein Fehler aufgetreten ist 
      * (nach {@link #spooler_close()}, vor {@link #spooler_exit()}).
      */

    public void     spooler_on_error    ()      throws Exception  {}

    
    /*+ Wird als letzte Funktion einer fehlerlosen Task gerufen (nach {@link #spooler_close()}, vor {@link #spooler_exit()}).
      */

    public void     spooler_on_success  ()      throws Exception  {}



    //private native void spooler_method_not_implemented();


    
    /*+ Zum Protokollieren.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "Eine Debug-Ausgabe" );
     * </pre>
     * 
     * @see Log#debug(String)
     */
    public Log      spooler_log;

    
    /*+ Das Objekt der Task.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "Meine Task-Id ist " + spooler_task.id() );
     * </pre>
     * 
     * <p><br/><b>Beispiel für JavaScript</b>
     * <pre>
     *     spooler_log.debug( "Meine Task-Id ist " + spooler_task.id );
     * </pre>
     * 
     * @see Task#id()
     */
    public Task     spooler_task;


    
    /*+ Das Objekt des Jobs.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "Der Jobname ist " + spooler_job.name() );
     * </pre>
     * 
     * <p><br/><b>Beispiel für JavaScript</b>
     * <pre>
     *     spooler_log.debug( "Der Jobname ist " + spooler_job.name );
     * </pre>
     * 
     * @see Job#name()
     */
    public Job      spooler_job;

    
    //public Thread   spooler_thread;

    
    /*+ Das Objekt des Schedulers.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "Das Startverzeichnis des Schedulers ist " + spooler.directory() );
     * </pre>
     * 
     * <p><br/><b>Beispiel für JavaScript</b>
     * <pre>
     *     spooler_log.debug( "Das Startverzeichnis des Schedulers ist " + spooler.directory );
     * </pre>
     * 
     * @see Spooler#directory()
     */
    public Spooler  spooler;
}
