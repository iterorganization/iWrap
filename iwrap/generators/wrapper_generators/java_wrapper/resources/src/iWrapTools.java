import imasjava.imas;
import imasjava.wrapper.LowLevel;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

public class iWrapTools{

    static String readFile(String path)
            throws IOException
    {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, StandardCharsets.UTF_8);
    }

    static public IDSDescription[] read_input(String file_name, int array_expected_size) throws IOException {

        int readSize = -1;
        IDSDescription idsDescriptions[];

        if (array_expected_size < 1)
        {
            idsDescriptions = new IDSDescription[0];
            return idsDescriptions;
        }

        FileReader fr = new FileReader(file_name);
        BufferedReader br = new BufferedReader(fr);
        String line;

        line = br.readLine();  // skip line " === Arguments ===="
        line = br.readLine();  // skip line " Length:"
        line = br.readLine();  //length
        readSize = Integer.parseInt(line);

        if ( array_expected_size != readSize)
        {
            throw new IOException("ERROR: Expected and read number of arguments differs ( " +  array_expected_size + " vs " + readSize);
        }

        idsDescriptions = new IDSDescription[readSize];
        for (int i=0; i < idsDescriptions.length; i++)
        {
            IDSDescription idsDescription = new IDSDescription();

            idsDescription.read(br);
            idsDescriptions[i] = idsDescription;
        }

        br.close();
        fr.close();

        return idsDescriptions;
    }

    public static String read_code_parameters() throws IOException {
        String xmlString=iWrapTools.readFile("code_parameters.xml");
        return xmlString;
    }

    static public void write_output(String fileName, int statusCode, String statusMessage) throws IOException
    {
        BufferedWriter writer = new BufferedWriter(new FileWriter(fileName, true));
        writer.write("" + statusCode);
        writer.newLine();

        if ( statusMessage != null )
        {
            int str_len =  statusMessage.length();
            writer.write("" + str_len);
            writer.newLine();
            writer.write(statusMessage);
            writer.newLine();
        }
        else
        {
            writer.write("0");
            writer.newLine();
            writer.write("");
            writer.newLine();
        }

        writer.close();
    }

    static public void handle_status_info(int status_code, String status_message, String actor_name, String method_name)
    {
        String NO_MSG = "<No diagnostic message>";
        if(status_message == null)
            status_message = NO_MSG;

        if(status_code !=0) {
          System.out.println("---Diagnostic information returned from *** " + actor_name + " / " + method_name + "***:---\n");
          System.out.println("-------Status code    : " + status_code + "\n");
          System.out.println("-------Status message : " + status_message + "\n" );
          System.out.println("---------------------------------------------------------\n");
        }

    }

    static public void open_db(IDSDescription idsDescription) throws Exception {

        int idx = -1;

        if (idsDescription.backend_id == LowLevel.MEMORY_BACKEND)
            return;

        idx = imas.openEnv(idsDescription.pulse,
                idsDescription.run,
                idsDescription.user,
                idsDescription.database,
                idsDescription.version,
                idsDescription.backend_id);

        idsDescription.idx = idx;
    }

    static public void close_db(IDSDescription idsDescription) {

            if (idsDescription.backend_id == LowLevel.MEMORY_BACKEND)
                return;

            try{
                imas.close(idsDescription.idx);
            } catch (Exception ex){
                // Nothing to do here... It is just cleaning operation.
            }
    }

}
