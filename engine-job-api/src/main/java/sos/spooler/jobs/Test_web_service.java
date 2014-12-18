// $Id$
/**
 * 
 * @author Joacim Zschimmer
 * @version $Revision$  $Date$
 * Created on 19.01.2006
 *
 */
package sos.spooler.jobs;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

import org.apache.xerces.parsers.DOMParser;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import sos.spooler.jobs.Web_service_forwarder.Order_exception;



/** 
 * @author Joacim Zschimmer
 * @version $Revision: 3946 $
 */


public class Test_web_service  extends sos.spooler.Job_impl
{
    //private String              current_url_string = null;
    public final String encoding = "utf-8"; //"iso-8859-1";
            

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
    
    public Test_web_service()  throws Exception
    {
    }
    
    //---------------------------------------------------------------------------------------------

    public boolean spooler_process()  throws Exception
    {
        Document document                = dom_from_xml( (String)spooler_task.order().payload() );
        Element  service_request_element = document.getDocumentElement();
        Element  content_element         = null; 
        
        if( service_request_element == null )  throw new Exception( "<service_request> fehlt" );
        
        URL url = new URL( service_request_element.getAttribute( "url" ) );
        
        for( Node node = service_request_element.getFirstChild();; node = node.getNextSibling() )
        {
            if( node.getNodeType() == Node.ELEMENT_NODE  &&  node.getNodeName().equals( "content" ) )
            {
                content_element = (Element)node;
                break;
            }
        }

        if( content_element == null )  throw new Exception( "<content> fehlt" );

        //URL url = new URL( "http://localhost:" + spooler.tcp_port() + "/webdienst" );
            

        HttpURLConnection http_connection = null;
        
        try
        {
            spooler_log.info( "url=" + url );
            
            URLConnection connection = url.openConnection();
            if( !( connection instanceof HttpURLConnection ) )  throw new Exception( "Nur HTTP-URLs werden akzeptiert: url=" + url );

            http_connection = (HttpURLConnection)connection;
            http_connection.setRequestMethod( "POST" );
            http_connection.setRequestProperty( "Content-Type", "text/xml; charset=" + encoding );  //TODO scheduler_http.cxx muss charset verstehen!
            http_connection.setDoOutput( true );
            http_connection.connect();

            
            OutputStream        output_stream = http_connection.getOutputStream();
            //OutputStreamWriter  writer        = new OutputStreamWriter( output_stream, encoding );

            Element data_element = get_data_element( service_request_element );
            new XMLSerializer( output_stream, new OutputFormat( service_request_element.getOwnerDocument(), encoding, false ) )
            .serialize( data_element );
            
            //writer.write( "<?xml version='1.0'?>\n" );
            //writer.write( "<my_request>bla äöü </my_request>\n" );
            //writer.flush();
            output_stream.flush();
            

            int response_code = http_connection.getResponseCode();
            
            if( response_code != HttpURLConnection.HTTP_OK 
             && response_code != HttpURLConnection.HTTP_ACCEPTED )
                throw new Exception( http_connection.getResponseMessage() );
            
            //Object response = http_connection.getContent();
            spooler_log.info( string_from_reader( new InputStreamReader( http_connection.getInputStream(), encoding ) ) );        
        }
        finally
        {
            if( http_connection != null )  http_connection.disconnect();
        }

        return false;
    }
    
    //---------------------------------------------------------------------------------------------
    
    private static Document dom_from_xml( String xml )  throws Exception
    {
        DOMParser parser = new DOMParser();
        parser.parse( new org.xml.sax.InputSource( new StringReader( xml ) ) );
            
        return parser.getDocument();
    }

    //-----------------------------------------------------------------------------------------
    
    Element get_data_element( Element service_request_element )  throws Exception
    {
        Element content_element = element_after_comment_and_empty_text( service_request_element.getFirstChild() );
        if( content_element == null )  throw new Exception( "<service_request> ohne <content>" );
        
        Element data_element = element_after_comment_and_empty_text( content_element.getFirstChild() );
        if( data_element == null )  throw new Exception( "<content> ist leer" );
        if( element_after_comment_and_empty_text( data_element.getNextSibling() ) != null )  throw new Exception( "<content> enthält mehr als einen Knoten" );
        
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

    //---------------------------------------------------------------------------------------------
}
