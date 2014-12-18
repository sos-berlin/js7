// $Id: Task.java 3946 2005-09-26 08:52:01Z jz $

package sos.spooler.jobs;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import javax.xml.transform.TransformerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

// xercesImpl.jar 
import org.apache.xerces.parsers.DOMParser;
import org.apache.xml.serialize.OutputFormat;  
import org.apache.xml.serialize.XMLSerializer; 

// sos.spooler.jar
import sos.spooler.Order;



/** 
 * @author Joacim Zschimmer
 * @version $Revision: 3946 $
 */


public class Web_service_forwarder  extends sos.spooler.Job_impl
{
    //private String              current_url_string = null;
    public final String encoding = "utf-8"; //"iso-8859-1";
            
    TransformerFactory transformer_factory = TransformerFactory.newInstance();
    
    //---------------------------------------------------------------------------------------------
    /** Führt zu order.setback() */
    
    static class Order_exception  extends Exception
    {
        /**
         * 
         */
        private static final long serialVersionUID = 2672152684233523911L;

        Order_exception( String message ) 
        {
            super( message );
        }

        Order_exception( String message, Exception x ) 
        {
            super( message, x );
        }
    }
    
    //---------------------------------------------------------------------------------------------
    /** Eine Auftragsausführung */
    
    class Process
    {
        Order       order       = spooler_task.order();
        
        //-----------------------------------------------------------------------------------------
        
        Process()  throws Exception
        {
        }
        
        //-----------------------------------------------------------------------------------------
        
        void process()  throws Exception
        {
            /*
            Object payload = order.payload();
            
            if( !( payload instanceof String ) )  
            {
                spooler_log.warn( "Order.payload ist kein String" );
                return false;
            }
            */
            
            //Web_service web_service = order.web_service();
            //File stylesheet_file = new File( web_service.forward_xslt_stylesheet_path() );
            //Document web_service_request_document = apply_xslt_stylesheet( stylesheet_file, dom_from_xml( order.xml() ) ); 
            //execute_service( web_service_request_document  );
            
            Object payload = order.payload();
            if( !(payload instanceof String ) )  throw new Order_exception( "order.payload ist kein String" );
            
            execute_service( dom_from_xml( (String)payload ) );
        }
        
        //-----------------------------------------------------------------------------------------
        
        private Document dom_from_xml( String xml )  throws Order_exception
        {
            try
            {
                DOMParser parser = new DOMParser();
                //parser.setIncludeIgnorableWhitespace( false );
                //parser.setErrorHandler();
                parser.parse( new org.xml.sax.InputSource( new StringReader( xml ) ) );
                    
                return parser.getDocument();
            }
            catch( Exception x )
            {
                throw new Order_exception( "Error while reading XML: " + x, x );
            }
        }

        //-----------------------------------------------------------------------------------------
        /*
        private Document apply_xslt_stylesheet( File stylesheet_file, Document order_document )  throws Order_exception
        {
            try
            {
                Transformer transformer = transformer_factory.newTransformer( new StreamSource( stylesheet_file ) );
                
                Document result = new DocumentImpl();
                transformer.transform( new DOMSource( order_document ), new DOMResult( result ) );
                
                return result;
            }
            catch( Exception x )
            {
                throw new Order_exception( "Error while transforming: " + x, x );
            }
        }
        */
        //-----------------------------------------------------------------------------------------
        
        private void execute_service( Document service_request_document )  throws Exception
        {
            Element service_request_element = service_request_document.getDocumentElement();

            if( service_request_element == null || !service_request_element.getNodeName().equals( "service_request" ) )
                throw new Order_exception( "Payload ist kein <service_request>" );
    
            
            HttpURLConnection http_connection = null;
            
            try
            {
                URL url = new URL( service_request_element.getAttribute( "url" ) );
                
                URLConnection connection = url.openConnection();
                if( !( connection instanceof HttpURLConnection ) )  throw new Order_exception( "Nur HTTP-URLs werden akzeptiert: url=" + url );

                http_connection = (HttpURLConnection)connection;
                http_connection.setRequestMethod( "POST" );
                http_connection.setRequestProperty( "Content-Type", "text/xml; charset=" + encoding );  //TODO scheduler_http.cxx muss charset verstehen!
                http_connection.setUseCaches( false );
                http_connection.setDoOutput( true );
                
                spooler_log.debug3( "Connecting with " + url );
                http_connection.connect();
    
                
                OutputStream        output_stream = http_connection.getOutputStream();
                OutputStreamWriter  writer        = new OutputStreamWriter( output_stream, encoding );

                Element data_element = get_data_element( service_request_element );
                
              //if( data_node.getNodeType() == Node.TEXT_NODE 
              // || data_node.getNodeType() == Node.CDATA_SECTION_NODE )
              //{
              //    writer.write( data_node.getTextContent() );   // getTextContent() ist erst in einer neueren Version vorhanden
              //}
              //else
                if( data_element != null ) //  &&  data_element.getNodeType() == Node.ELEMENT_NODE )
                {
                    new XMLSerializer( output_stream, new OutputFormat( service_request_element.getOwnerDocument(), encoding, false ) )
                    .serialize( data_element );
                }
                else
                    throw new Order_exception( "<content> enthält kein Element" );
                    //throw new Order_exception( "<content> enthält kein Element und keinen Text" );
                    
                
                writer.close();
                output_stream.close();

                //spooler_log.debug3( "HTTP-output_stream closed" );

                
                
                // ANTWORT LESEN
                
                
                int response_code = http_connection.getResponseCode();

                spooler_log.debug3( "response_code=" + response_code );
                
                if( response_code != HttpURLConnection.HTTP_OK 
                 && response_code != HttpURLConnection.HTTP_ACCEPTED )
                    throw new Order_exception( "Response code " + response_code + " " + http_connection.getResponseMessage() );
                
                //Object response = http_connection.getContent();
                spooler_log.info( "Antwort des Web-Dienstes:" );
                spooler_log.info( string_from_reader( new InputStreamReader( http_connection.getInputStream(), encoding ) ) );        
            }
            finally
            {
                if( http_connection != null )  http_connection.disconnect();
            }
        }

        //-----------------------------------------------------------------------------------------
        
        Element get_data_element( Element service_request_element )  throws Order_exception
        {
            Element content_element = element_after_comment_and_empty_text( service_request_element.getFirstChild() );
            if( content_element == null )  throw new Order_exception( "<service_request> ohne <content>" );
            
            Element data_element = element_after_comment_and_empty_text( content_element.getFirstChild() );
            if( data_element == null )  throw new Order_exception( "<content> ist leer" );
            if( element_after_comment_and_empty_text( data_element.getNextSibling() ) != null )  throw new Order_exception( "<content> enthält mehr als einen Knoten" );
            
            //spooler_log.debug3( "data_node=" + data_node );
            
            return data_element;
        }        
        
        //-----------------------------------------------------------------------------------------
        
        Element element_after_comment_and_empty_text( Node node )  throws Order_exception
        {
            for(; node != null; node = node.getNextSibling() )
            {
                if( node.getNodeType() == Node.COMMENT_NODE )  continue; 
    
                if( ( node.getNodeType() == Node.TEXT_NODE || node.getNodeType() == Node.CDATA_SECTION_NODE )
                    &&  node.getNodeValue().trim().equals( "" ) )  continue;
                
                break;
            }
            
            if( node != null  &&  node.getNodeType() != Node.ELEMENT_NODE )  throw new Order_exception( "Unzulässiger Knoten: " + node );
            
            return (Element)node;
        }
    
        //-----------------------------------------------------------------------------------------
        
    } //class

    //---------------------------------------------------------------------------------------------
    
    static String string_from_reader( Reader reader )  throws IOException
    {
        StringBuffer result = new StringBuffer( 10000 );
        char[]       buffer = new char[ 1024 ];
        
        while(true)
        {
            int len = reader.read( buffer );
            if( len <= 0 )  break;
            result.append( buffer, 0, len );
        }
        
        return result.toString();
    }
    
    //---------------------------------------------------------------------------------------------
    
    public Web_service_forwarder()  throws Exception
    {
    }
    
    //---------------------------------------------------------------------------------------------
    
    public boolean spooler_open()  throws Exception
    {
        return true;
    }
    
    //---------------------------------------------------------------------------------------------
    
    public void spooler_close()
    {
        //if( http_connection != null )
        //{
        //    http_connection.disconnect();
        //}
    }
    
    //---------------------------------------------------------------------------------------------

    public boolean spooler_process()  throws Exception
    {
        try
        {
            new Process().process();
        }
        //catch( Order_exception x )
        catch( Exception x )
        {
            //spooler_log.warn( x.toString() );   printStackTrace() gibt das auch aus

            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            x.printStackTrace( new PrintStream( buffer ) );
            spooler_log.warn( buffer.toString() );
            
            spooler_task.order().setback();
        }

        return true;
    }
    
    //---------------------------------------------------------------------------------------------
}
