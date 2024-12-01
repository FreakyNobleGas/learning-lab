package testclasses;

import datamodels.Classroom;
import exceptionhandlers.InvalidDataException;

/**
 * This test class contains a single method, "testClassroomCreation" which takes
 * 3 arguments, a room type, room number, room capacity. Note that the
 * exceptions are thrown by the setters in the data model classes. If a setter
 * in the data model class throws an error, the set method exits with an error
 * and the error is returned to the testClassroomCreation method which handles
 * the error by printing out an error message.
 *
 * @author EKramer
 */
public class TestErrorHandlingClassroom {

    public static void main(String[] args) {

        // Create a classroom and the method arguments to be used for testing
        Classroom classroom = new Classroom();
        String roomNumber;
        String roomType;
        int capacity;
        int testNumber;

        /**
         * Test 1 - passing an empty string for room number
         */
        testNumber = 1;
        roomNumber = "";
        roomType = "LAB";
        capacity = 10;
        testCreateClassroom(testNumber, classroom, roomNumber, roomType, capacity);

        /**
         * Test 2 - passing an invalid string for room number
         */
        testNumber = 2;
        roomNumber = "FA";
        roomType = "LAB";
        capacity = 10;
        testCreateClassroom(testNumber, classroom, roomNumber, roomType, capacity);

        /**
         * Test 3 - passing an empty string for room type
         */
        testNumber = 3;
        roomNumber = "FA333";
        roomType = "";
        capacity = 10;
        testCreateClassroom(testNumber, classroom, roomNumber, roomType, capacity);

        /**
         * Test 4 - passing an invalid string for room type
         */
        testNumber = 4;
        roomNumber = "FA333";
        roomType = "HALLWAY";
        capacity = 10;
        testCreateClassroom(testNumber, classroom, roomNumber, roomType, capacity);

        /**
         * Test 5 - passing a negative int for room capacity
         */
        testNumber = 5;
        roomNumber = "FA333";
        roomType = "LAB";
        capacity = -10;
        testCreateClassroom(testNumber, classroom, roomNumber, roomType, capacity);
    }

    /**
     * This test method will try to create a classroom. If one of the data model
     * class set methods fails and throws an exception, the exception is caught
     * by the test method and the error is printed out. If the try block in the
     * test method succeeds, a classroom is created and the classroom
     * information is printed to the console.
     *
     * @param classroom
     * @param roomNumber
     * @param roomType
     * @param capacity
     */
    private static void testCreateClassroom(int testNumber, Classroom classroom, String roomNumber,
            String roomType, int capacity) {

        try {
            classroom.setRoomNumber(roomNumber);
            classroom.setTypeOfRoom(roomType);
            classroom.setCapacity(capacity);
            // This line of code is only executed if the try block succeeds
            System.out.println(classroom.toString());
        } catch (InvalidDataException exp) {
            System.out.println("Test #" + testNumber + ":" + exp.getMessage());
        }
    }

}
