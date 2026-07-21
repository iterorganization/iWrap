
import java.io.*;

public class IDSDescription {
    public String ids_type;
    public int occurrence;
    public String uri;

    /** Runtime-only open handle — not serialized. Set by {@code iWrapTools.open_db}. */
    public int idx = -1;

    public IDSDescription() {
    }

    public String toString() {
        return "IDSDescription{ids_type='" + this.ids_type + '\''
                            + ", occurrence=" + this.occurrence
                            + ", uri='" + this.uri + '\'' + '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        IDSDescription that = (IDSDescription) o;
        return occurrence == that.occurrence
               && ids_type.equals(that.ids_type)
               && uri.equals(that.uri);
    }

    public void read(BufferedReader br) throws IOException {

        String line;
        line = br.readLine(); // skip line " === IDS ===="

        line = br.readLine(); // IDS type
        this.ids_type = line.trim();

        line = br.readLine(); // occurrence
        this.occurrence = Integer.parseInt(line.trim());

        line = br.readLine(); // URI
        this.uri = line.trim();
    }
}