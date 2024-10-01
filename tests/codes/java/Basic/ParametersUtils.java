package iwrap.test_codes.basic;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import org.json.JSONArray;
import org.json.JSONObject;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import java.io.StringReader;
import java.io.IOException;

public class ParametersUtils
{
    // =====================================================================================
    //             HELPER FUNCTIONS
    // =====================================================================================

    public static Number[] extract_values(String parameters) throws SAXException, ParserConfigurationException, IOException
    {
        // returns [<ntimes>,<multiplication_factor>] from parameters string
        // handles xml and json parameters formats
        Number[] extracted_values = new Number[2];

        if(parameters.charAt(0) == '<')
        {
            return extract_xml_values(parameters);
        }
        else if(parameters.charAt(0) == '{')
        {
            return extract_json_values(parameters);
        }
        else if(parameters.charAt(0) == '&')
        {
           System.out.println("Fortran namelist parameters are supported only by fortran-based actors");
        }
        else
        {
            System.out.println("YAML parameters not supported yet");
        }

        return extracted_values;
    }

    public static Number[] extract_xml_values(String parameters) throws SAXException, ParserConfigurationException, IOException
    {
        // returns [<ntimes>,<multiplication_factor>] from parameters string
        Number[] extracted_values = new Number[2];

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(new InputSource(new StringReader(parameters)));
        Element rootElement = document.getDocumentElement();

        extracted_values[0] = Integer.valueOf(rootElement.getElementsByTagName("ntimes").item(0).getTextContent().trim());
        extracted_values[1] = Double.valueOf(rootElement.getElementsByTagName("multiplication_factor").item(0).getTextContent().trim());

        return extracted_values;
    }

    public static Number[] extract_json_values(String parameters)
    {
        // returns [<ntimes>,<multiplication_factor>] from parameters string
        Number[] extracted_values = new Number[2];

        JSONObject obj = new JSONObject(parameters);
        JSONObject parameters_obj = obj.getJSONObject("parameters");
        extracted_values[0] = parameters_obj.getInt("ntimes");
        extracted_values[1] = parameters_obj.getFloat("multiplication_factor");

        return extracted_values;
    }

}
