// $Id: Mail.java 3946 2005-09-26 08:52:01Z jz $

package sos.spooler;

/*+ eMail-Versand eines Protokolls.
 *
 * @see Log#mail()
 */

import java.io.File;

/**
 * @author Joacim Zschimmer
 * @version $Revision $
 */

public class Mail extends Idispatch implements HasBean<MailBean>
{
    public Mail(Invoker invoker) {
        super(invoker);
    }

    private             Mail                ( long idispatch )                              { super(idispatch); }

    
    
    /*+ Stellt den Empfänger ein.
     *
     * <p>Wenn die Hostware zum eMail-Versand JavaMail verwendet, können Sie mehere Adressen durch Komma getrennt angeben. 
     *    Siehe <a href="http://java.sun.com/products/javamail/javadocs/javax/mail/internet/InternetAddress.html#parse(java.lang.String)">javax.mail.InternetAddress.parse(String)</a>.
     * </p>
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_to( "admin@company.com" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.to = "admin@company.com";                                                  
     * </pre>
     * 
     * @param receipients
     */
    public void     set_to                  ( String receipients )                          {                   com_call( ">to", receipients        ); }
    
    
    
    /*+ Der Empfänger.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "to=" + spooler_log.mail().to() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "to=" + spooler_log.mail.to );                                                  
     * </pre>
     */
    @SchedulerGetter
    public String       to                  ()                                              { return (String)   com_call( "<to"                     ); }

    
    
    /*+ Stellt den Absender ein.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_from( "scheduler@company.com" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.from = "scheduler@company.com";                                                  
     * </pre>
     * 
     * @param from
     */
    public void     set_from                ( String from )                                 {                   com_call( ">from", from             ); }
    
    
    
    /*+ Der Absender.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( spooler_log.mail().from() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( spooler_log.mail.from );                                                  
     * </pre>
     * 
     */
    @SchedulerGetter
    public String       from                ()                                              { return (String)   com_call( "<from"                   ); }

    
    
    /*+ Stellt den Empfänger einer Kopie ein.
     * 
     * <p>Wenn die Hostware zum eMail-Versand JavaMail verwendet, können Sie mehere Adressen durch Komma getrennt angeben. 
     *    Siehe <a href="http://java.sun.com/products/javamail/javadocs/javax/mail/internet/InternetAddress.html#parse(java.lang.String)">javax.mail.InternetAddress.parse(String)</a>.
     * </p>
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_cc( "hans@company.com" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.cc = "hans@company.com";                                                  
     * </pre>
     */
    public void     set_cc                  ( String receipients )                          {                   com_call( ">cc", receipients        ); }
    
    
    
    /*+ Empfänger einer Kopie.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "cc=" + spooler_log.mail().cc() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "cc=" + spooler_log.mail.cc );                                                  
     * </pre>
     */
    @SchedulerGetter
    public String       cc                  ()                                              { return (String)   com_call( "<cc"                     ); }

    
    
    /*+ Stellt den nicht sichtbaren Empfänger einer Kopie ein.
     * 
     * <p>Wenn die Hostware zum eMail-Versand JavaMail verwendet, können Sie mehere Adressen durch Komma getrennt angeben. 
     *    Siehe <a href="http://java.sun.com/products/javamail/javadocs/javax/mail/internet/InternetAddress.html#parse(java.lang.String)">javax.mail.InternetAddress.parse(String)</a>.
     * </p>
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_bcc( "admin2@company.com" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.bcc = "admin2@company.com";                                                  
     * </pre>
     */
    public void     set_bcc                 ( String receipients )                          {                   com_call( ">bcc", receipients       ); }
    
    
    
    /*+ Der nicht sichtbare Empfänger einer Kopie.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "bcc=" + spooler_log.mail().bcc() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "bcc=" + spooler_log.mail.bcc );                                                  
     * </pre>
     */
    @SchedulerGetter
    public String       bcc                 ()                                              { return (String)   com_call( "<bcc"                    ); }

    
    
    /*+ Stellt die Betreffzeile ein.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_subject( "Job X war erfolgreich" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.subject = "Job X war erfolgreich";                                     
     * </pre>
     * 
     */
    public void     set_subject             ( String text )                                 {                   com_call( ">subject", text          ); }
    
    
    
    /*+ Die Betreffzeile.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "subject=" + spooler_log.mail().subject() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "subject=" + spooler_log.mail.subject );                                                  
     * </pre>
     */
    @SchedulerGetter
    public String       subject             ()                                              { return (String)   com_call( "<subject"                ); }

    
    
    /*+ Stellt die Nachricht ein.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_body( "Der Job lief\nerfolgreich" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.body = "Der Job lief\nerfolgreich";                                                  
     * </pre>
     * 
     * @param text
     */
    public void     set_body                ( String text )                                 {                   com_call( ">body", text             ); }
    
    
    
    /*+ Die Nachricht.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "body=" + spooler_log.mail().body() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "body=" + spooler_log.mail.body );                                                  
     * </pre>
     * 
     */
    @SchedulerGetter
    public String       body                ()                                              { return (String)   com_call( "<body"                   ); }

    
    
    /*+ Hängt der Nachricht eine Datei an.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().add_file( "c:/tmp/1.txt", "1.txt", "text/plain", "quoted-printable" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.add_file( "c:/tmp/1.txt", "1.txt", "text/plain", "quoted-printable" );                                                  
     * </pre>
     * 
     * @param pathname Pfadname der Datei
     * @param mail_filename Dateiname, wie er in der Nachricht erscheinen soll (default ist der Dateiname aus dem pathname).
     * @param content_type Z.B. "text/plain" (default)
     * @param encoding "quoted-printable"
     */
    public void         add_file            ( String pathname, String mail_filename, 
                                              String content_type, String encoding )        {                   com_call( "add_file", pathname, mail_filename,
                                                                                                                                      content_type, encoding ); }

    /*+ Hängt der Nachricht eine Datei an.
     * 
     * Dasselbe wie add_file( pathname, mail_filename, content_type, "" );
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().add_file( "c:/tmp/1.txt", "1.txt", "text/plain" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.add_file( "c:/tmp/1.txt", "1.txt", "text/plain" );                                                  
     * </pre>
     * 
     * @see #add_file(String,String,String,String)
     */
    public void         add_file            ( String real_filename, String mail_filename, 
                                              String content_type )                         {                   com_call( "add_file", real_filename, mail_filename, content_type ); }

    
    /*+ Hängt der Nachricht eine Datei an.
     * 
     * Dasselbe wie add_file( pathname, mail_filename, "", "" );
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().add_file( "c:/tmp/1.txt", "1.txt" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.add_file( "c:/tmp/1.txt", "1.txt" );  
     * </pre>
     * 
     * @see #add_file(String,String,String,String)
     */
    public void         add_file            ( String real_filename, String mail_filename )  {                   com_call( "add_file", real_filename, mail_filename ); }

    
    
    /*+ Hängt der Nachricht eine Datei an.
     * 
     * Dasselbe wie add_file( pathname, "", "", "" );
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().add_file( "c:/tmp/1.txt" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.add_file( "c:/tmp/1.txt" );  
     * </pre>
     * 
     * @see #add_file(String,String,String,String)
     */
    public void         add_file            ( String real_filename )                        {                   com_call( "add_file", real_filename ); }

    
    
    /*+ Stellt den SMTP-Server ein.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_smtp( "mail.company.com" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.smtp = "mail.company.com";                                                  
     * </pre>
     * 
     * @param hostname Name oder IP-Adresse des Servers.
     */

    public void     set_smtp                ( String hostname )                             {                   com_call( ">smtp", hostname         ); }
    
    
    
    /*+ Liefert den Namen oder die IP-Adresse des SMTP-Server.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "smtp=" + spooler_log.mail().smtp() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "smtp=" + spooler_log.mail.smtp );                                                  
     * </pre>
     */
    @SchedulerGetter
    public String       smtp                ()                                              { return (String)   com_call( "<smtp"                   ); }

    
    
    /*+ Stellt das Verzeichnis für noch nicht versendbare Nachrichten ein.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_queue_dir( "c:/tmp/email" );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail().queue_dir = "c:/tmp/email";                                                  
     * </pre>
     * 
     * @param directory
     */
    public void     set_queue_dir           ( String directory )                            {                   com_call( ">queue_dir", directory   ); }

    
    
    /*+ Liefert das Verzeichnis für noch nicht versendbare Nachrichten.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.debug( "queue_dir=" + spooler_log.mail().queue_dir() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.debug( "queue_dir=" + spooler_log.mail.queue_dir );                                                  
     * </pre>
     * 
     */
    @SchedulerGetter
    public String       queue_dir           ()                                              { return (String)   com_call( "<queue_dir"              ); }

    
    
    /*+ Fügt der Nachricht eine Kopfzeile hinzu.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().add_header_field( "X-Header", "4711" );                                                  
     * </pre>
     *
     * @param field_name
     * @param value
     */
    public void         add_header_field    ( String field_name, String value )             {                   com_call( "add_header_field", field_name, value ); }

    
    
    /*+ Versendet die Nachrichten aus dem Verzeichnis der noch nicht versendeten Nachrichten.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().dequeue();                                                  
     * </pre>
     * 
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail().dequeue();                                                  
     * </pre>
     * 
     * @return Anzahl der versendeten Nachrichten.
     *
     */
    public int          dequeue             ()                                              { return        int_com_call( "dequeue"                 ); }

    
    
    /*+ Protokoll von dequeue().
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().dequeue();                                                  
     *     spooler_log.debug( "queue-log:\n" + spooler_log.mail().dequeue_log() );                                                  
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.dequeue();                                                  
     *     spooler_log.debug( "queue-log:\n" + spooler_log.mail.dequeue_log );                                                  
     * </pre>
     * 
     * @return Protokoll (mehrzeilig).
     */
    @SchedulerGetter
    public String       dequeue_log         ()                                              { return (String)   com_call( "<dequeue_log"            ); }
    
    
    /*+ XSLT-Stylesheet für eMail-Aufbereitung setzen.
     * 
     * <p><br/><b>Beispiel</b>
     * <pre>
     *     spooler_log.mail().set_xslt_stylesheet_path( "c:/stylesheets/mail.xslt" );
     * </pre>
     *
     * <p><br/><b>Beispiel in JavaScript</b>
     * <pre>
     *     spooler_log.mail.xslt_stylesheet_path = "c:/stylesheets/mail.xslt";
     * </pre>
     *
     * @see #set_xslt_stylesheet_path(String)
     */
    public void         set_xslt_stylesheet_path( String path )                             {                   com_call( ">xslt_stylesheet_path", path ); }



    /*+ XSLT-Stylesheet für eMail-Aufbereitung setzen.
     * @see #set_xslt_stylesheet_path(String)
     */
    public void         set_xslt_stylesheet_path( File file )                               {                   com_call( ">xslt_stylesheet_path", file.toString() ); }



    /*+ XSLT-Stylesheet für eMail-Aufbereitung.
     * @see #set_xslt_stylesheet_path(String)
     */
    @SchedulerGetter
    public String       xslt_stylesheet_path()                                              { return (String)   com_call( "<xslt_stylesheet_path"   ); }

    @Override public final MailBean toBean() {
        return new MailBean(this);
    }
}
