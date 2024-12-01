package testclasses;

import datacontainers.ClassroomDC;
import datamodels.Classroom;
import exceptionhandlers.InvalidDataException;

import java.util.ArrayList;

/**
 * This test class will test creation of data containers and storage into those
 * containers
 * 
 * @author ekramer
 */
public class TestDataContainers {

    public static void main(String[] args) {

        try {
			// Create data container
			ClassroomDC classroomDataContainer = new ClassroomDC();
			
			// Create a classroom 
			Classroom classroom1 = new Classroom();
			classroom1.setRoomNumber("EBD3310");
			classroom1.setTypeOfRoom("LAB");
			classroom1.setCapacity(10);
			// Add to data container
			classroomDataContainer.getListOfClassrooms().add(classroom1);

			// Create a classroom 
			Classroom classroom2 = new Classroom();
			classroom2.setRoomNumber("QED6712");
			classroom2.setTypeOfRoom("LAB");
			classroom2.setCapacity(30);
			// Add to data container
			classroomDataContainer.getListOfClassrooms().add(classroom2);

			// Create a classroom 
			Classroom classroom3 = new Classroom();
			classroom3.setRoomNumber("TYU9000");
			classroom3.setTypeOfRoom("LECTURE HALL");
			classroom3.setCapacity(200);
			// Add to data container
			classroomDataContainer.getListOfClassrooms().add(classroom3);

			// Print out all the objects in the data container
			ArrayList<Classroom> listOfClassroomsInContainer = classroomDataContainer.getListOfClassrooms();

			// If you haven't seen this type of for loop before, it is referred to 
			// as a "for each" loop.  It loops through the list of retrieved classrooms
			// and stores each one in "oneClassroom" through each iteration of the loop.
			for (Classroom oneClassroom : listOfClassroomsInContainer) {
			    System.out.println(oneClassroom.toString());
			}
		} catch (InvalidDataException e) {
			e.printStackTrace();
		}
    }
}