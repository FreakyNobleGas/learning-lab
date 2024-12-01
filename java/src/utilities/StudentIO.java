/*
 *  This Class contains methods to write out the student objects in several different formats
 */
package utilities;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import datacontainers.StudentDC;
import datamodels.Course;
import datamodels.Student;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
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

public class StudentIO implements Serializable {

    /**
     * Constructor is declared private because the IO classes are utilities
     * which contain static methods and should never be instantiated
     */
    private StudentIO() {
    }

    /**
     * Writes out a text file containing all students in the student data model
     */
    public static void writeTextFile(String fileLocation, StudentDC datacontainer) {

        PrintWriter textFile = null;

        try {
            // Create output file
            // We are putting it in a location specified when the program is run
            // This is done via a command line argument
            textFile = new PrintWriter(fileLocation + "students.txt");

            // Loop through the array list of students and print delimited text to a file
            for (Student student : datacontainer.getListOfStudents()) {
                textFile.println(student.getName()
                        + ":" + student.getAddress()
                        + ":" + student.getGPA()
                        + ":" + student.getDateOfBirth()
                        + ":" + student.getDateOfGraduation()
                        + ":" + student.getStudentID());
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
     * Creates a serialized object output file containing all classrooms in the
     * studentdata model
     */
    public static void writeSerializedFile(String fileLocation, StudentDC datacontainer) {
        try {
            // Create output file
            ObjectOutputStream serializedFile = new ObjectOutputStream(
                    new FileOutputStream(fileLocation + "students.ser"));
            // Write out the data
            serializedFile.writeObject(datacontainer.getListOfStudents());
        } catch (Exception exp) {
            // TO-DO
        }
    }

    /**
     * Writes out the student data in XML format containing all classrooms in
     * the student data model
     */
    public static void writeXMLFile(String fileLocation, StudentDC datacontainer) {

        // get a document builder factory
        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();

        try {
            // get a document builder from the factory
            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();

            // create an instance of the document model
            Document doc = docBuilder.newDocument();

            // create the root element <list_of_students> and append to document
            Element root = doc.createElement("list_of_students");
            doc.appendChild(root);

            for (Student student : datacontainer.getListOfStudents()) {

                Element studentElement = doc.createElement("student");
                root.appendChild(studentElement);

                // Name tag
                Element nameElement = doc.createElement("name");
                // Name tag text
                Text nameText = doc.createTextNode(student.getName());
                // Append name text to name tag
                nameElement.appendChild(nameText);
                // Append name tag to student element
                studentElement.appendChild(nameElement);

                // Address tag
                Element addressElement = doc.createElement("address");
                // Address tag text
                Text addressText = doc.createTextNode(student.getAddress());
                // Append address text to address tag
                addressElement.appendChild(addressText);
                // Append address tag to student element
                studentElement.appendChild(addressElement);

                // student id tag
                Element studentid = doc.createElement("student_id");
                // student id text for tag
                Text studentID = doc.createTextNode(String.valueOf(student.getStudentID()));
                // Append text to tag
                studentid.appendChild(studentID);
                // Append tag to student element
                studentElement.appendChild(studentid);

                // birth date tag
                Element dob = doc.createElement("birth_date");
                // birth date text
                Text dob_text = doc.createTextNode(student.getDateOfBirth().format(DateTimeFormatter.ISO_DATE));
                // Append text to tag
                dob.appendChild(dob_text);
                // Append tag to student element
                studentElement.appendChild(dob);

                // graduation date tag
                Element dog = doc.createElement("graduation_date");
                // graduation data text
                Text dog_text = doc.createTextNode(student.getDateOfGraduation().format(DateTimeFormatter.ISO_DATE));
                // Append text to tag
                dog.appendChild(dog_text);
                // Append tag to student element
                studentElement.appendChild(dog);

                // GPA tag
                Element gpa = doc.createElement("gpa");
                // GPA data text - converting float to a string
                Text gpa_text = doc.createTextNode(String.valueOf(student.getGPA()));
                // Append text to tag
                gpa.appendChild(gpa_text);
                // Append tag to student element
                studentElement.appendChild(gpa);

                // Student courses will be saved individually under the student_courses list tag
                Element studentcourses = doc.createElement("student_courses");
                for (Course courseinfo : student.getListOfCourses()) {
                    // course tag
                    Element course = doc.createElement("course");
                    // course id tag
                    Element courseid = doc.createElement("course_id");
                    // course id text
                    Text courseidtext = doc.createTextNode(courseinfo.getCourseID());
                    // Append course id text to course id tag
                    courseid.appendChild(courseidtext);
                    // Append course id tag to course element
                    course.appendChild(courseid);
                    // Append course tag to studentcourses element
                    studentcourses.appendChild(course);
                }
                // Append student_courses tag to student tag
                studentElement.appendChild(studentcourses);
            }

            try {
                Transformer tr = TransformerFactory.newInstance().newTransformer();
                tr.setOutputProperty(OutputKeys.ENCODING, "UTF-8");

                // send DOM to file
                tr.transform(new DOMSource(doc),
                        new StreamResult(new FileOutputStream(fileLocation + "students.xml")));

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
     * Writes out the student data in JSON format containing all students in the
     * student data model
     *
     */
    public static void writeJSONFile(String fileLocation, StudentDC datacontainer) {

        PrintWriter jsonFile = null;

        try {
            // Create output file
            jsonFile = new PrintWriter(fileLocation + "students.json");

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // Convert studentlist to JSON format
            gson.toJson(datacontainer.getListOfStudents(), jsonFile);

        } catch (Exception exp) {
            // TO-DO
        } finally {
            // Flush the output stream and close the file
            jsonFile.flush();
            jsonFile.close();
        }
    }

    /**
     * Reads a JSON formatted file of students
     *
     */
    public static ArrayList<Student> readJSONFile(String fileLocation) {

        ArrayList<Student> listOfStudents = new ArrayList<>();

        try {
            // Create input file
            BufferedReader jsonFile = new BufferedReader(new FileReader(fileLocation + "students.json"));

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // fromJson returns an array
            Student[] studentArray = gson.fromJson(jsonFile, Student[].class);

            // Convert to arraylist for the data model
            for (int i = 0; i < studentArray.length; i++) {
                listOfStudents.add(studentArray[i]);
            }
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }
        return listOfStudents;

    }

    /**
     * Read a delimited text file, the delimeter is ":". You need to know the
     * format of the text file in order to parse it correctly.
     */
    public static ArrayList<Student> readTextFile(String fileLocation) {

        ArrayList<Student> listOfStudents = new ArrayList<>();

        try {

            // Keep track of when we reach the end of the file
            boolean eof = false;

            // Set the input file
            BufferedReader bw = new BufferedReader(new FileReader(fileLocation + "students.txt"));

            // While there are still rows in the input file, read one in
            while (!eof) {

                Student studentobject = new Student();

                String lineFromFile = bw.readLine();
                if (lineFromFile == null) {
                    eof = true;
                } else {
                    // Split the input string into elements
                    String[] lineElements = lineFromFile.split(":");

                    // Save in the student object
                    studentobject.setName(lineElements[0]);
                    studentobject.setAddress(lineElements[1]);
                    studentobject.setGPA(Float.parseFloat(lineElements[2]));
                    // Convert the date strings to a LocalDate
                    LocalDate tmpdob = parseDateString(lineElements[3]);
                    studentobject.setDateOfBirth(tmpdob);
                    LocalDate tmpdog = parseDateString(lineElements[4]);
                    studentobject.setDateOfGraduation(tmpdog);
                    studentobject.setStudentID(lineElements[5]);

                    // add student to the data container 
                    listOfStudents.add(studentobject);
                }
            }
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }

        return listOfStudents;
    }

    /**
     * Reads an XML formatted file of students
     */
    public static ArrayList<Student> readXMLFile(String fileLocation) {

        ArrayList<Student> listOfStudents = new ArrayList<>();

        try {

            // Get the factory instance
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

            //Using factory, get an instance of document builder
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

            //parse using builder to get document representation of the XML file
            Document xmlDocument = documentBuilder.parse(fileLocation + "students.xml");

            //get the root elememt (list_of_students)
            Element list_of_students = xmlDocument.getDocumentElement();

            //retrieve the list of students from the root of the document
            NodeList studentList = list_of_students.getElementsByTagName("student");

            //loop through the list of students and create student objects            
            for (int i = 0; i < studentList.getLength(); i++) {

                //get a student element from the list
                Element studentElement = (Element) studentList.item(i);

                //get the data for the student, we retrieve node lists for convenience
                //but we will only have one of each so we will use the first element in 
                // each list
                NodeList nameList = studentElement.getElementsByTagName("name");
                NodeList addressList = studentElement.getElementsByTagName("address");
                NodeList birthdateList = studentElement.getElementsByTagName("birth_date");
                NodeList graddateList = studentElement.getElementsByTagName("graduation_date");
                NodeList studentIdList = studentElement.getElementsByTagName("student_id");
                NodeList gpaList = studentElement.getElementsByTagName("gpa");

                //create a student object
                Student newstudent = new Student();

                //retrieve name and get its content (text value)
                String name = nameList.item(0).getTextContent();

                //set the value in the student
                newstudent.setName(name);

                //retrieve address and get its content (text value)
                String address = addressList.item(0).getTextContent();

                //set the value in the student
                newstudent.setAddress(address);

                // retrieve the date strings and convert to LocalDate objects
                // using the helper method
                LocalDate tmpdob = parseDateString(birthdateList.item(0).getTextContent());
                newstudent.setDateOfBirth(tmpdob);
                LocalDate tmpdog = parseDateString(graddateList.item(0).getTextContent());
                newstudent.setDateOfGraduation(tmpdog);

                // get gpa and convert to float
                float gpa = Float.parseFloat(gpaList.item(0).getTextContent());
                newstudent.setGPA(gpa);

                // get student id
                String studentid = studentIdList.item(0).getTextContent();
                newstudent.setStudentID(studentid);

                // store in array list
                listOfStudents.add(newstudent);
            }
        } // if wrong file name is entered, let Main Menu handle it
        catch (Exception exp) {
            System.out.println(exp.getMessage());
        }

        return listOfStudents;

    }

    /**
     * Reads a serialized list of Students
     */
    public static ArrayList<Student> readSerializedFile(String fileLocation) {

        ArrayList<Student> listOfStudents = new ArrayList<>();

        try {
            ObjectInputStream ois = new ObjectInputStream(
                    new FileInputStream("students.ser"));
            listOfStudents = (ArrayList<Student>) ois.readObject();
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        return listOfStudents;
    }

    /**
     * private helper method to convert the stored date string to a local date
     * object - it's a bit of an effort to convert the date so it's good to make
     * a separate method. Ideally you should put this in a utility class so it
     * can be reused. There's a lot going on here. To see the individual steps,
     * refer to the same method in the StudentIO class
     */
    private static LocalDate parseDateString(String dateString) {
        String dateElements[] = dateString.split("-");
        return LocalDate.of(Integer.parseInt(dateElements[0]),
                Integer.parseInt(dateElements[1]),
                Integer.parseInt(dateElements[2]));
    }
}
