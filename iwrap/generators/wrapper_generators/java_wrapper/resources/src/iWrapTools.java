import imasjava.imas;

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

    static public void handle_status_info(int status_code, String status_message, String actor_name)
    {
  /*
    const char* NO_MSG = "<No diagnostic message>";
    if(status_message == null)
    {
        int msg_size = strlen(NO_MSG) + 1;
        status_message = (char*) malloc(msg_size);
        strcpy(status_message, NO_MSG);
    }

    if(status_code !=0) {
      printf("---Diagnostic information returned from *** %s ***:---\n", actor_name);
      printf("-------Status code    : %d\n", status_code);
      printf("-------Status message : %s\n", status_message);
      printf("---------------------------------------------------------\n");
      }
      */
    }




    static public void open_db_entries(IDSDescription idsDescriptions[]) throws Exception {
        // List<IDSDescription> list = Arrays.stream(idsDescriptions).distinct().collect(Collectors.toList());
        // TODO Add recycling of already opened DB Entry
        for (IDSDescription idsDescription: idsDescriptions)
        {
            idsDescription.idx = imas.openEnv(idsDescription.shot,
                    idsDescription.run,
                    idsDescription.user,
                    idsDescription.database,
                    idsDescription.version);

        }
    }

    static public void closeDBEntries(IDSDescription idsDescriptions[]) {
        Integer[] idxArr = Arrays.stream(idsDescriptions)
                .map(e -> e.idx)
                .distinct()
                .toArray(Integer[]::new);

        for (int idx : idxArr)
        {
            try{
                imas.close(idx);
            } catch (Exception ex){
                // Nothing to do here... It is just cleaning operation.
            }
        }

    }

    public static void main(String[] args) {

        IDSDescription idsDescriptions[];

        try {
            idsDescriptions = iWrapTools.read_input("/pfs/work/g2bpalak/IWRAP_SANDBOX/basic_methods_java_1-34435/main.in",
                    2);

            System.out.println(Arrays.toString(idsDescriptions));
        }
        catch(Exception exc)
            {
                exc.printStackTrace();
            }
    }
}
