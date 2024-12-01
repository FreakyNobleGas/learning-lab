// Name: Nicholas Quinn
//
// Assignment 2

package testclasses;

import datamodels.Classroom;
import exceptionhandlers.InvalidDataException;

public class TestClassroomCreation {

    public static void main(String[] args) {
        
        try {
			// Create a classroom 
			Classroom classroom = new Classroom();
			classroom.setRoomNumber("THX1138");
			classroom.setTypeOfRoom("CB");
			classroom.setCapacity(30);
			System.out.println(classroom.toString());
			
		} catch (InvalidDataException e) {
			e.printStackTrace();
		}
       
    }
}
