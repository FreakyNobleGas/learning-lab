/*
 *  This Class contains methods to write out the faculty objects in several different formats
 */
package utilities;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import datacontainers.FacultyDC;
import datamodels.Course;
import datamodels.Faculty;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

public class FacultyIO implements Serializable {

    /**
     * Constructor is declared private because the IO classes are utilities
     * which contain static methods and should never be instantiated
     */
    private FacultyIO() {
    }

    /**
     * Writes out a text file containing all faculty in the faculty data model
     */
    public static void writeTextFile(String fileLocation, FacultyDC datacontainer) {

        PrintWriter textFile = null;

        try {
            // Create output file
            // We are putting it in a location specified when the program is run
            // This is done via a command line argument
            textFile = new PrintWriter(fileLocation + "faculty.txt");

            // Loop through the array list of faculty and print delimited text to a file
            for (Faculty faculty : datacontainer.getListOfFaculty()) {
                textFile.println(faculty.getName()
                        + ":" + faculty.getAddress()
                        + ":" + faculty.getDateOfBirth()
                        + ":" + faculty.getDateOfHire()
                        + ":" + faculty.getStatus()
                        + ":" + faculty.getSalary()
                        + ":" + faculty.getListOfCourses());
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
        } finally {
            // Flush the output stream and close the file
            textFile.flush();
            textFile.close();
        }
    }

    /**
     * Creates a serialized object output file containing all faculty in the
     * faculty data model
     */
    public static void writeSerializedFile(String fileLocation, FacultyDC datacontainer) {
        try {
            // Create output file
            ObjectOutputStream serializedFile = new ObjectOutputStream(
                    new FileOutputStream(fileLocation + "faculty.ser"));
            // Write out the data
            serializedFile.writeObject(datacontainer.getListOfFaculty());
        } catch (Exception exp) {
            // TO-DO
        }
    }

    /**
     * Writes out the faculty data in XML format containing all facultys in the
     * faculty data model
     */
    public static void writeXMLFile(String fileLocation, FacultyDC datacontainer) {

        try {

            // get a document builder factory
            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();

            // get a document builder from the factory
            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();

            // create an instance of the document model
            Document doc = docBuilder.newDocument();

            // create the root element <list_of_facultys> and append to document
            Element root = doc.createElement("list_of_faculty");
            doc.appendChild(root);

            for (Faculty faculty : datacontainer.getListOfFaculty()) {

                Element facultyElement = doc.createElement("faculty");
                root.appendChild(facultyElement);

                // Name
                Element nameElement = doc.createElement("name");
                Text nameText = doc.createTextNode(faculty.getName());
                nameElement.appendChild(nameText);
                facultyElement.appendChild(nameElement);

                // Address
                Element addressElement = doc.createElement("address");
                Text addressText = doc.createTextNode(faculty.getAddress());
                addressElement.appendChild(addressText);
                facultyElement.appendChild(addressElement);

                // birth_date 
                Element dob = doc.createElement("birth_date");
                // graduation data text
                Text dob_text = doc.createTextNode(faculty.getDateOfBirth().format(DateTimeFormatter.ISO_DATE));
                // Append text to tag
                dob.appendChild(dob_text);
                // Append tag to student element
                facultyElement.appendChild(dob);

                // hire_date 
                Element doh = doc.createElement("hire_date");
                // graduation data text
                Text doh_text = doc.createTextNode(faculty.getDateOfHire().format(DateTimeFormatter.ISO_DATE));
                // Append text to tag
                doh.appendChild(doh_text);
                // Append tag to student element
                facultyElement.appendChild(doh);

                // salary - will also convert this to string the same way
                Element sal = doc.createElement("salary");
                Text sal_text = doc.createTextNode(faculty.getSalary() + "");
                sal.appendChild(sal_text);
                facultyElement.appendChild(sal);

                // status
                Element stat = doc.createElement("status");
                Text stat_text = doc.createTextNode(faculty.getStatus().toString());
                stat.appendChild(stat_text);
                facultyElement.appendChild(stat);

                // faculty_courses, stored as a comma delimited list
                Element classes = doc.createElement("faculty_courses");
                for (Course facultyCourse : faculty.getListOfCourses()) {
                    String str = "";
                    str = str + facultyCourse.getCourseID() + ",";
                    Text classes_text = doc.createTextNode(str.toString());
                    classes.appendChild(classes_text);
                    facultyElement.appendChild(classes);
                }

            }
            try {
                Transformer tr = TransformerFactory.newInstance().newTransformer();
                tr.setOutputProperty(OutputKeys.ENCODING, "UTF-8");

                // send DOM to file
                tr.transform(new DOMSource(doc),
                        new StreamResult(new FileOutputStream(fileLocation + "faculty.xml")));

            } catch (TransformerException te) {
                System.out.println(te.getMessage());
            } catch (IOException ioe) {
                System.out.println(ioe.getMessage());
            }
        } catch (ParserConfigurationException pce) {
            System.out.println("Error trying to instantiate DocumentBuilder " + pce);
        }
    }

    /**
     * Writes out the faculty data in JSON format containing all faculty in the
     * faculty data model
     *
     */
    public static void writeJSONFile(String fileLocation, FacultyDC datacontainer) {

        PrintWriter jsonFile = null;

        try {
            // Create output file
            jsonFile = new PrintWriter(fileLocation + "faculty.json");

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // Convert facultylist to JSON format
            gson.toJson(datacontainer.getListOfFaculty(), jsonFile);

        } catch (Exception exp) {
            // TO-DO
        } finally {
            // Flush the output stream and close the file
            jsonFile.flush();
            jsonFile.close();
        }
    }

    /**
     * Reads a serialized list of faculty
     */
    public static ArrayList<Faculty> readSerializedFile(String fileLocation) {

        ArrayList<Faculty> listOfFaculty = new ArrayList<>();

        try {
            ObjectInputStream ois = new ObjectInputStream(
                    new FileInputStream("faculty.ser"));
            listOfFaculty = (ArrayList<Faculty>) ois.readObject();
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        return listOfFaculty;
    }

    /**
     * Read a delimited text file, the delimeter is ":". You need to know the
     * format of the text file in order to parse it correctly.
     */
    public static ArrayList<Faculty> readTextFile(String fileLocation) {

        ArrayList<Faculty> listOfFaculty = new ArrayList<>();

        try {

            // Keep track of when we reach the end of the file
            boolean eof = false;

            // Set the input file
            BufferedReader bw = new BufferedReader(new FileReader(fileLocation + "faculty.txt"));

            // While there are still rows in the input file, read one in
            while (!eof) {

                Faculty facultyobject = new Faculty();

                String lineFromFile = bw.readLine();
                if (lineFromFile == null) {
                    eof = true;
                } else {
                    // Split the input string into elements
                    String[] lineElements = lineFromFile.split(":");

                    // Save in the faculty object
                    facultyobject.setName(lineElements[0]);
                    facultyobject.setAddress(lineElements[1]);
                    // Convert the date strings to a LocalDate
                    LocalDate tmpdob = parseDateString(lineElements[2]);
                    facultyobject.setDateOfBirth(tmpdob);
                    LocalDate tmpdoh = parseDateString(lineElements[3]);
                    facultyobject.setDateOfHire(tmpdoh);
                    facultyobject.setStatus(lineElements[4]);
                    facultyobject.setSalary(Double.parseDouble(lineElements[5]));

                    // add faculty to the data container 
                    listOfFaculty.add(facultyobject);
                }
            }
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }

        return listOfFaculty;
    }

    /**
     * Reads an XML formatted file of faculty
     */
    public static ArrayList<Faculty> readXMLFile(String fileLocation) {

        ArrayList<Faculty> listOfFaculty = new ArrayList<>();

        try {

            // Get the factory instance
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

            //Using factory, get an instance of document builder
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

            //parse using builder to get document representation of the XML file
            Document xmlDocument = documentBuilder.parse(fileLocation + "faculty.xml");

            //get the root elememt (list_of_facultys)
            Element list_of_faculty = xmlDocument.getDocumentElement();

            //retrieve the list of facultys from the root of the document
            NodeList facultyList = list_of_faculty.getElementsByTagName("faculty");

            //loop through the list of faculty and create faculty objects            
            for (int i = 0; i < facultyList.getLength(); i++) {

                //get a faculty element from the list
                Element facultyElement = (Element) facultyList.item(i);

                //get the data for the faculty, we retrieve node lists for convenience
                //but we will only have one of each so we will use the first element in 
                // each list
                NodeList nameList = facultyElement.getElementsByTagName("name");
                NodeList addressList = facultyElement.getElementsByTagName("address");
                NodeList birthdateList = facultyElement.getElementsByTagName("birth_date");
                NodeList hiredateList = facultyElement.getElementsByTagName("hire_date");
                NodeList salaryList = facultyElement.getElementsByTagName("salary");
                NodeList statusList = facultyElement.getElementsByTagName("status");

                //create a faculty object
                Faculty newfaculty = new Faculty();

                //retrieve name and get its content (text value)
                String name = nameList.item(0).getTextContent();

                //set the value in the faculty
                newfaculty.setName(name);

                //retrieve address and get its content (text value)
                String address = addressList.item(0).getTextContent();

                //set the value in the faculty
                newfaculty.setAddress(address);
                
                // retrieve the date strings and convert to LocalDate objects
                // using the helper method
                LocalDate tmpdob = parseDateString(birthdateList.item(0).getTextContent());
                newfaculty.setDateOfBirth(tmpdob);
                LocalDate tmpdoh = parseDateString(hiredateList.item(0).getTextContent());
                newfaculty.setDateOfHire(tmpdoh);

                // get salary and convert to double
                double salary = Double.parseDouble(salaryList.item(0).getTextContent());
                newfaculty.setSalary(salary);

                // get status
                String status = statusList.item(0).getTextContent();
                newfaculty.setStatus(status);

                // store in array list
                listOfFaculty.add(newfaculty);
            }
        } // if wrong file name is entered, let Main Menu handle it
        catch (Exception exp) {
            System.out.println(exp.getMessage());
        }

        return listOfFaculty;

    }

    /**
     * Reads a JSON formatted file of classrooms and returns an array list of
     * classrooms.
     *
     */
    public static ArrayList<Faculty> readJSONFile(String fileLocation) {

        ArrayList<Faculty> listOfFaculty = new ArrayList<>();

        try {
            // Create input file
            BufferedReader jsonFile = new BufferedReader(new FileReader(fileLocation + "faculty.json"));

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // fromJson returns an array
            Faculty[] facultyArray = gson.fromJson(jsonFile, Faculty[].class);

            // Convert to arraylist for the data model
            for (int i = 0; i < facultyArray.length; i++) {
                listOfFaculty.add(facultyArray[i]);
            }
        } catch (Exception exp) {
             System.out.println(exp.getMessage());
        }
        return listOfFaculty;

    }

    /**
     * private helper method to convert the stored date string to a local date
     * object - it's a bit of an effort to convert the date so it's good to make
     * a separate method. Ideally you should put this in a utility class so it
     * can be reused. I've implemented all the steps separately so you can see
     * what's going on but you can combine all the steps. For an example of
     * combining the steps, refer to the parseDateString method in StudentIO
     */
    private static LocalDate parseDateString(String dateString) {
        String dateElements[] = dateString.split("-");
        int year = Integer.parseInt(dateElements[0]);
        int month = Integer.parseInt(dateElements[1]);
        int day = Integer.parseInt(dateElements[2]);
        LocalDate date = LocalDate.of(year, month, day);
        return date;
    }
}
