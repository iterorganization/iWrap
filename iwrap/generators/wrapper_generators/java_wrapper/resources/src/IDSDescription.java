
import java.io.*;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class IDSDescription {
    public String ids_type;
    public int shot;
    public int run;
    public int occurrence;
    public int backend_id;
    public int idx;
    public String database;
    public String user;
    public String version;

    public IDSDescription() {
    }

    public String toString() {
        return "IDSDescription{ids_type='" + this.ids_type + '\''
                            + ", shot=" + this.shot
                            + ", run=" + this.run
                            + ", occurrence=" + this.occurrence
                            + ", backend_id=" + this.backend_id
                            + ", idx=" + this.idx
                            + ", database='" + this.database + '\''
                            + ", user='" + this.user + '\''
                            + ", version='" + this.version + '\'' + '}';
    }

    public boolean db_equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        IDSDescription that = (IDSDescription) o;
        return shot == that.shot
            && run == that.run && idx == that.idx && backend_id == that.backend_id
            && database.equals(that.database) && user.equals(that.user) && version.equals(that.version);
    }

       @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        IDSDescription that = (IDSDescription) o;
        return this.db_equals(that)
            && occurrence == that.occurrence
               && ids_type.equals(that.ids_type) ;
    }


    public void read(BufferedReader br) throws IOException {

        String line;
        line = br.readLine(); // skip line " === IDS ===="

        line = br.readLine(); //IDS type
        this.ids_type = line.trim();


        line = br.readLine(); // shot
        this.shot = Integer.parseInt(line);


        line = br.readLine(); // run
        this.run =Integer.parseInt(line);

        line = br.readLine(); //occurrence
        this.occurrence = Integer.parseInt(line);

        line = br.readLine(); //backend
        this.backend_id = Integer.parseInt(line);

        line = br.readLine(); //idx
        this.idx = Integer.parseInt(line);


        line = br.readLine(); // database
        this.database = line.trim();


        line = br.readLine();  // user
        this.user = line.trim();


        line = br.readLine(); // version
        this.version = line.trim();
}
}