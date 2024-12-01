/*
 *  This Class contains methods to write out the course objects in several different formats
 */
package utilities;

import datacontainers.CourseDC;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import datacontainers.ClassroomDC;
import datamodels.Classroom;
import datamodels.Course;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.Serializable;
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

public class CourseIO implements Serializable {

    /**
     * Constructor is declared private because the IO classes are utilities
     * which contain static methods and should never be instantiated
     */
    private CourseIO() {
    }

    /**
     * Writes out a text file containing all courses in the course data model
     *
     */
    public static void writeTextFile(String fileLocation, CourseDC datacontainer) {

        PrintWriter textFile = null;

        try {
            // Create output file
            // We are putting it in a location specified when the program is run
            // This is done via a command line argument
            textFile = new PrintWriter(fileLocation + "courses.txt");

            // Loop through the array list of classrooms and print delimited text to a file
            for (Course course : datacontainer.getListOfCourses()) {
                textFile.println(course.getCourseID() + ":" + course.getCourseName()
                        + ":" + course.getClassroom().getRoomNumber());
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
     * Creates a serialized object output file containing all courses in the
     * course data model
     */
    public static void writeSerializedFile(String fileLocation, CourseDC datacontainer) {
        try {
            // Create output file
            ObjectOutputStream serializedFile = new ObjectOutputStream(
                    new FileOutputStream(fileLocation + "courses.ser"));
            // Write out the data
            serializedFile.writeObject(datacontainer.getListOfCourses());
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }
    }

    /**
     * Writes out the course data in XML format containing all courses in the
     * course data model
     */
    public static void writeXMLFile(String fileLocation, CourseDC datacontainer) {

        // get a document builder factory
        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();

        try {
            // get a document builder from the factory
            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();

            // create an instance of the document model
            Document doc = docBuilder.newDocument();

            // create the root element <list_of_classrooms> and append to document
            Element root = doc.createElement("list_of_courses");
            doc.appendChild(root);

            // Loop through the array list of courses and create the course elements of the xml file
            for (Course a_course : datacontainer.getListOfCourses()) {

                Element course = doc.createElement("course");
                root.appendChild(course);

                // Name
                Element nameElement = doc.createElement("course_name");
                Text nameText = doc.createTextNode(a_course.getCourseName());
                nameElement.appendChild(nameText);
                course.appendChild(nameElement);

                // ID
                Element addressElement = doc.createElement("course_id");
                Text idText = doc.createTextNode(a_course.getCourseID());
                addressElement.appendChild(idText);
                course.appendChild(addressElement);

                // Classroom
                Element classroomElement = doc.createElement("classroom");
                Text classroomText = doc.createTextNode(a_course.getClassroom().getRoomNumber());
                classroomElement.appendChild(classroomText);
                course.appendChild(classroomElement);
            }

            try {
                Transformer tr = TransformerFactory.newInstance().newTransformer();
                tr.setOutputProperty(OutputKeys.ENCODING, "UTF-8");

                // send DOM to file
                tr.transform(new DOMSource(doc),
                        new StreamResult(new FileOutputStream(fileLocation + "courses.xml")));

            } catch (TransformerException te) {
                System.out.println(te.getMessage());
            } catch (IOException ioe) {
                System.out.println(ioe.getMessage());
            }
        } catch (ParserConfigurationException exp) {
            System.out.println(exp.getMessage());
        }
    }

    /**
     * Writes out the course data in JSON format containing all courses in the
     * course data model
     *
     */
    public static void writeJSONFile(String fileLocation, CourseDC datacontainer) {

        PrintWriter jsonFile = null;

        try {
            // Create output file
            jsonFile = new PrintWriter(fileLocation + "courses.json");

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // Convert classroom list to JSON format
            gson.toJson(datacontainer.getListOfCourses(), jsonFile);

        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        } finally {
            // Flush the output stream and close the file
            jsonFile.flush();
            jsonFile.close();
        }
    }

    /**
     * Reads a JSON formatted file of offered courses
     *
     */
    public static ArrayList<Course> readJSONFile(String fileLocation) {

        ArrayList<Course> listOfCourses = new ArrayList<>();

        try {
            // Create input file
            BufferedReader jsonFile = new BufferedReader(new FileReader(fileLocation + "courses.json"));

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // fromJson returns an array
            Course[] coursesArray = gson.fromJson(jsonFile, Course[].class);

            // Convert to arraylist for the data model
            for (int i = 0; i < coursesArray.length; i++) {
                listOfCourses.add(coursesArray[i]);
            }
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }

        return listOfCourses;
    }

    /**
     * Reads a set of course objects from a serialized file 
     */
    public static ArrayList<Course> readSerializedFile(String fileLocation) {

        ArrayList<Course> listOfCourses = new ArrayList<>();

        try {
            ObjectInputStream serializedFile = new ObjectInputStream(
                    new FileInputStream(fileLocation + "courses.ser"));
            // Read the serialized object and cast to its original type
            listOfCourses = (ArrayList<Course>) serializedFile.readObject();
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }
        return listOfCourses;
    }

    /**
     * Reads an XML formatted file of courses
     */
    public static ArrayList<Course> readXMLFile(String fileLocation, ClassroomDC datacontainer) {

        ArrayList<Course> listOfCourses = new ArrayList<>();

        try {

            // Get the factory instance
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

            //Using factory, get an instance of document builder
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

            //parse using builder to get document representation of the XML file
            Document xmlDocument = documentBuilder.parse(fileLocation + "courses.xml");

            //get the root elememt (list_of_courses)
            Element list_of_courses = xmlDocument.getDocumentElement();

            //retrieve the list of courses from the root of the document
            NodeList courseList = list_of_courses.getElementsByTagName("course");

            //loop through the list of courses and create course objects            
            for (int i = 0; i < courseList.getLength(); i++) {

                // Create a course object
                Course course = new Course();

                //get a course element from the list
                Element courseElement = (Element) courseList.item(i);

                //get the data for the course, we retrieve node lists for convenience
                //but we will only have one of each so we will use the first element in 
                // each list
                NodeList courseNameList = courseElement.getElementsByTagName("course_name");
                //retrieve the first element (course name) and get its content (text value)
                String courseName = courseNameList.item(0).getTextContent();
                //set the value in the course
                course.setCourseName(courseName);

                // Get the course id and set its value
                NodeList courseIdList = courseElement.getElementsByTagName("course_id");
                String courseId = courseIdList.item(0).getTextContent();
                //set the value in the course
                course.setCourseID(courseId);

                // Get the classroom
                // Only the classroom number is stored so we have to retrieve the actual classroom
                // reference from the classroom container
                NodeList classroom = courseElement.getElementsByTagName("classoom");

                for (Classroom aRoom : datacontainer.getListOfClassrooms()) {
                    if (aRoom.getRoomNumber().equals(classroom)) {
                        course.setClassroom(aRoom);
                    }
                }

                //add the classroom to the data model arraylist
                listOfCourses.add(course);
            }
        } // if wrong file name is entered, let Main Menu handle it
        catch (Exception exp) {
            // TO-DO
        }

        return listOfCourses;

    }

    /**
     * Reads a delimited text file of courses 
     *
     * An end of file flag is used to keep track of whether we hit the end of
     * the file, It starts out false and if we hit the end of file (null input),
     * it changes to true and execution stops.
     *
     * The format of the text file is:
     *
     * Example: INFO301:Class name:classroom
     */
    public static ArrayList<Course> readTextFile(String fileLocation, ClassroomDC datacontainer) {

        ArrayList<Course> listOfCourses = new ArrayList<>();

        try {
            boolean eof = false;
            BufferedReader textFile = new BufferedReader(new FileReader(fileLocation + "courses.txt"));
            while (!eof) {
                String lineFromFile = textFile.readLine();
                if (lineFromFile == null) {
                    eof = true;
                } else {
                    // Create a course
                    Course course = new Course();

                    // Split the input line into classroom elements using the delimiter
                    String[] lineElements = lineFromFile.split(":");

                    // The first element is the course number
                    course.setCourseID(lineElements[0]);

                    // The second element is the course name
                    course.setCourseName(lineElements[1]);

                    // The third element is the classroom
                    // Get the classroom
                    // Only the classroom number is stored so we have to retrieve the actual classroom
                    // reference from the classroom container               
                    for (Classroom aRoom : datacontainer.getListOfClassrooms()) {
                        if (aRoom.getRoomNumber().equals(lineElements[2])) {
                            course.setClassroom(aRoom);
                        }
                    }
                    // add the classroom to the arraylist
                    listOfCourses.add(course);
                }
            }
        } catch (Exception exp) {
            System.out.println(exp.getMessage());
        }
        return listOfCourses;
    }
    
  
}
