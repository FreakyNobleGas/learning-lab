/*
 *  This Class contains methods to write out the classroom objects in several different formats
 *  and read in the data in the same formats.
 */
package utilities.io;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import datacontainers.ClassroomDC;
import datamodels.Classroom;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader; 
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

public class ClassroomIO {

    /**
     * Constructor is declared private because the IO classes are utilities
     * which contain static methods
     */
    private ClassroomIO() {
    }

    /**
     * Writes out a text file containing all classrooms in the classroom data
     * container
     *
     * The format of the text file is:
     *
     * Example: FA301,CLASSROOM
     */
    public static void writeTextFile(String fileLocation, ClassroomDC classroomDataContainer) {

        PrintWriter textFile = null;

        try {
            // Create output file
            // We are putting it in a location specified when the program is run
            // This is done via a command line argument
            textFile = new PrintWriter(fileLocation + "classroom.csv");

            // Loop through the array list of classrooms and print comma delimited text to a file
            for (Classroom classroom : classroomDataContainer.getListOfClassrooms()) {
                textFile.println(classroom.getRoomNumber() + "," + classroom.getTypeOfRoom());
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
     * classroom data container
     */
    public static void writeSerializedFile(String fileLocation, ClassroomDC classroomdatamodel) {
        try {
            // Create output file
            ObjectOutputStream serializedFile = new ObjectOutputStream(
                    new FileOutputStream(fileLocation + "classroom.ser"));
            // Write out the data
            serializedFile.writeObject(classroomdatamodel.getListOfClassrooms());
        } catch (Exception exp) {
            // TO-DO
        }
    }

    /**
     * Writes out the classroom data in XML format containing all classrooms in
     * the classroom data container
     */
    public static void writeXMLFile(String fileLocation, ClassroomDC classroomDataContainer) {

        // get a document builder factory
        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();

        try {
            // get a document builder from the factory
            DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();

            // create an instance of the document model
            Document doc = docBuilder.newDocument();

            // create the root element <list_of_classrooms> and append to document
            Element root = doc.createElement("list_of_classrooms");
            doc.appendChild(root);

            // Loop through the array list of classrooms and create the classroom elements of the xml file
            for (Classroom classroom : classroomDataContainer.getListOfClassrooms()) {

                Element classroomElement = doc.createElement("classroom");

                Element roomNumberElement = doc.createElement("room_number");
                Text roomNumberText = doc.createTextNode(classroom.getRoomNumber());
                roomNumberElement.appendChild(roomNumberText);
                classroomElement.appendChild(roomNumberElement);

                Element roomtypeElement = doc.createElement("room_type");
                Text roomtypetText = doc.createTextNode(classroom.getTypeOfRoom().toString());
                roomtypeElement.appendChild(roomtypetText);
                classroomElement.appendChild(roomtypeElement);
                
                Element capacityElement = doc.createElement("room_capacity");
                Text capacityText = doc.createTextNode(String.valueOf(classroom.getCapacity()));
                roomNumberElement.appendChild(capacityText);
                classroomElement.appendChild(capacityElement);

                root.appendChild(classroomElement);

            }

            // use default xml formatting in the file
            OutputFormat format = new OutputFormat(doc);
            format.setIndenting(true);

            // open the output stream
            XMLSerializer serializer = new XMLSerializer(
                    new FileOutputStream(new File(fileLocation + "classroom.xml")), format);

            // write out the object
            serializer.serialize(doc);

        } catch (Exception exp) {
            // TO-DO
        }
    }

    /**
     * Writes out the classroom data in JSON format containing all classrooms in
     * the classroom data container
     *
     */
    public static void writeJSONFile(String fileLocation, ClassroomDC classroomDataContainer) {

        PrintWriter jsonFile = null;

        try {
            // Create output file
            FileWriter fw = new FileWriter(fileLocation + "classroom.json", true);
            jsonFile = new PrintWriter(fw);

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // Convert classroom list to JSON format
            gson.toJson(classroomDataContainer.getListOfClassrooms(), jsonFile);

        } catch (FileNotFoundException exp) {
            // TO-DO
            } catch (IOException exp) {
        } finally {
            // Flush the output stream and close the file
            jsonFile.flush();
            jsonFile.close();
        }
    }

    /**
     * Reads a set of classroom objects from a serialized file and returns an
     * array list of classrooms
     */
    public static ArrayList<Classroom> readSerializedFile(String fileLocation) {

        ArrayList<Classroom> listOfClassrooms = new ArrayList<>();

        try {
            ObjectInputStream serializedFile = new ObjectInputStream(
                    new FileInputStream(fileLocation + "classroom.ser"));
            // Read the serialized object and cast to its original type
            listOfClassrooms = (ArrayList<Classroom>) serializedFile.readObject();
        } catch (Exception exp) {
            // TO-DO
        } finally {
            return listOfClassrooms;
        }
    }

    /**
     * Reads a comma delimited text file of classrooms and returns an array list of
     * classrooms.
     *
     * eof is used to keep track of whether we hit the end of the file, It
     * starts out false and if we hit the end of file (null input), it changes
     * to true and execution stops.
     *
     * The format of the text file is:
     *
     * Example: FA301:CLASSROOM
     */
    public static ArrayList<Classroom> readTextFile(String fileLocation) {

        ArrayList<Classroom> listOfClassrooms = new ArrayList<>();

        try {
            boolean eof = false;
            BufferedReader textFile = new BufferedReader(new FileReader(fileLocation + "classroom.csv"));
            while (!eof) {
                String lineFromFile = textFile.readLine();
                if (lineFromFile == null) {
                    eof = true;
                } else {
                    // Create a classroom
                    Classroom classroom = new Classroom();

                    // Split the input line into classroom elements using the delimiter
                    String[] lineElements = lineFromFile.split(",");

                    // The first element is the classroom number
                    classroom.setRoomNumber(lineElements[0]);

                    // The second element is the classroom type
                    classroom.setTypeOfRoom(lineElements[1]);
                    
                    // add the classroom to the arraylist
                    listOfClassrooms.add(classroom);
                }
            }
        } catch (Exception exp) {
            // TO-DO
        } finally {
            return listOfClassrooms;
        }
    }

    /**
     * Reads an XML formatted file of classrooms and returns an array list of
     * classrooms
     */
    public static ArrayList<Classroom> readXMLFile(String fileLocation) {

        ArrayList<Classroom> listOfClassrooms = new ArrayList<>();

        try {

            // Get the factory instance
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

            //Using factory, get an instance of document builder
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

            //parse using builder to get document representation of the XML file
            Document xmlDocument = documentBuilder.parse(fileLocation + "classroom.xml");

            //get the root elememt (list_of_classrooms)
            Element list_of_classrooms = xmlDocument.getDocumentElement();

            //retrieve the list of classrooms from the root of the document
            NodeList classroomList = list_of_classrooms.getElementsByTagName("classroom");

            //loop through the list of classrooms and create classroom objects            
            for (int i = 0; i < classroomList.getLength(); i++) {

                //get a classroom element from the list
                Element classroomElement = (Element) classroomList.item(i);

                //get the data for the classroom, we retrieve node lists for convenience
                //but we will only have one of each so we will use the first element in 
                // each list
                NodeList roomNumberList = classroomElement.getElementsByTagName("room_number");
                NodeList roomTypeList = classroomElement.getElementsByTagName("room_type");

                //create a classroom
                Classroom newclassroom = new Classroom();

                //retrieve the first element from the roomnumber list and get its content (text value)
                String roomnumber = roomNumberList.item(0).getTextContent();

                //set the value of room number in the classroom
                newclassroom.setRoomNumber(roomnumber);

                //retrieve the first element from the roomtype list and get its content (text value)
                String roomtype = roomTypeList.item(0).getTextContent();

                //set the value of type in the classroom object
                newclassroom.setTypeOfRoom(roomtype);                
                
                //add the classroom to the data model arraylist
                listOfClassrooms.add(newclassroom);
            }
        } // if wrong file name is entered, let Main Menu handle it
        catch (Exception exp) {
            // TO-DO
        } finally {
            return listOfClassrooms;
        }
    }

    /**
     * Reads a JSON formatted file of classrooms and returns an array list of
     * classrooms.
     *
     */
    public static ArrayList<Classroom> readJSONFile(String fileLocation) {

        ArrayList<Classroom> listOfClassrooms = new ArrayList<>();

        try {
            // Create input file
            BufferedReader jsonFile = new BufferedReader(new FileReader(fileLocation + "classroom.json"));

            // Create JSON object
            Gson gson = new GsonBuilder().create();

            // fromJson returns an array
            Classroom[] classroomArray = gson.fromJson(jsonFile, Classroom[].class);

            // Convert to arraylist for the data model
            for (int i = 0; i < classroomArray.length; i++) {
                listOfClassrooms.add(classroomArray[i]);
            }
        } catch (Exception exp) {
            // TO-DO
        } finally {
            return listOfClassrooms;
        }
    }
}
