package testclasses;

import datamodels.Classroom;
import datamodels.Course;
import datamodels.Faculty;
import datamodels.Student;
import exceptionhandlers.InvalidDataException;
import java.time.LocalDate;
import java.time.Month;

/**
 * This test class contains several methods that tests error handling which has
 * been implemented in the set methods of the data models. If a setter in a data
 * model class throws an error, the set method exits with an error and the error
 * is returned to the test method that was called, which handles the error by
 * printing out an error message.
 *
 * @author EKramer
 */
public class TestErrorHandlingAllClasses {
    
    public static void main(String[] args) {

        // Create a valid date which will be used in some test methods
        LocalDate testDate = LocalDate.of(2018, Month.MAY, 4);
        
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

        // Create a faculty and the method arguments to be used for testing        
        Faculty faculty = new Faculty();
        String name;
        String address;
        double salary;

        /**
         * Test 6 - passing an empty string for faculty name
         */
        testNumber = 6;
        name = "";
        address = "University Ave, Lowell, MA";
        salary = 10000;
        testCreateFaculty(testNumber, faculty, name, address, salary, testDate);
        
        /**
         * Test 7 - passing an empty string for faculty address
         */
        testNumber = 7;
        name = "John Jones";
        address = "";
        salary = 10000;
        testCreateFaculty(testNumber, faculty, name, address, salary, testDate);
        
        /**
         * Test 8 - passing a null for faculty date of birth
         */
        testNumber = 8;
        name = "John Jones";
        address = "University Ave, Lowell, MA";
        salary = 10000;
        testCreateFaculty(testNumber, faculty, name, address, salary, null);
        
        /**
         * Test 9 - passing a negative value for salary
         */
        testNumber = 9;
        name = "John Jones";
        address = "University Ave, Lowell, MA";
        salary = -10000;
        testCreateFaculty(testNumber, faculty, name, address, salary, testDate);
        
        /**
         * Test 10 - passing a zero for salary
         */
        testNumber = 10;
        name = "John Jones";
        address = "University Ave, Lowell, MA";
        salary = 0;
        testCreateFaculty(testNumber, faculty, name, address, salary, testDate);
        
        // Create a student and the method arguments to be used for testing        
        Student student = new Student();
        int studentIdInt=1234567;
        String studentIdString="0234567";
        float gpa=-0.0f;
        name = "John Jones";
        address = "University Ave, Lowell, MA";
       
        /**
         * Test 11 - passing an invalid gpa
         */
        testNumber = 11;
        testCreateStudent1(testNumber, student, name, address, studentIdInt, 
        testDate, testDate, gpa);
        
        /**
         * Test 12 - passing an invalid student id
         */
        testNumber = 12;
        studentIdInt=12345;
        testCreateStudent1(testNumber, student, name, address, studentIdInt, 
        testDate, testDate, gpa);
        
        /**
         * Test 13 - passing an invalid student id
         */
        testNumber = 13;
        testCreateStudent2(testNumber, student, name, address, studentIdString, 
        testDate, testDate, gpa);
        
        /**
         * Test 14 - passing an invalid student id
         */
        testNumber = 14;
        studentIdString="12345";
        gpa = 4.0f;
        testCreateStudent2(testNumber, student, name, address, studentIdString, 
        testDate, testDate, gpa);        
        
        /**
         * Test 15 - passing a null for student id
         */
        testNumber = 15;
        testCreateStudent2(testNumber, student, name, address, null, 
        testDate, testDate, gpa);
        
         // Create a course and the method arguments to be used for testing
        Course course = new Course();
        String courseID;
        String courseName = "Java Programming";
        Classroom room = new Classroom();
        
        /**
         * Test 16 - passing an invalid course id
         */
        testNumber = 16;
        courseID="301";
        testCreateCourse(testNumber, course, classroom, courseID, courseName);
        
        /**
         * Test 17 - passing no course id
         */
        testNumber = 17;
        courseID="";
        testCreateCourse(testNumber, course, classroom, courseID, courseName);
        
    }

    /**
     * This test method will try to create a classroom. If one of the data model
     * class set methods fails and throws an exception, the exception is caught
     * by the test method and the error is printed out. If the try block in the
     * test method succeeds, a classroom is created and the classroom
     * information is printed to the console.
     *
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

    /**
     * This test method will try to create a faculty. If one of the data model
     * class set methods fails and throws an exception, the exception is caught
     * by the test method and the error is printed out. If the try block in the
     * test method succeeds, a faculty is created and the faculty
     * information is printed to the console.
     *
     */
    private static void testCreateFaculty(int testNumber, Faculty faculty,
            String name, String address, double salary, LocalDate testDate) {
        
        try {
            faculty.setName(name);
            faculty.setAddress(address);
            faculty.setSalary(salary);
            faculty.setDateOfBirth(testDate);
            faculty.setDateOfHire(testDate);
            // This line of code is only executed if the try block succeeds
            System.out.println(faculty.toString());
        } catch (InvalidDataException exp) {
            System.out.println("Test #" + testNumber + ":" + exp.getMessage());
        }
    }

    /**
     * This test method will try to create a student. If one of the data model
     * class set methods fails and throws an exception, the exception is caught
     * by the test method and the error is printed out. If the try block in the
     * test method succeeds, a student is created and the student
     * information is printed to the console.
     *
     */
    private static void testCreateStudent1(int testNumber, Student student, 
            String name, String address, int studentid, LocalDate dateofbirth,
            LocalDate dateofgraduation, float gpa) {
        
        try {
            student.setName(name);
            student.setAddress(address);
            student.setStudentID(studentid);
            student.setDateOfBirth(dateofbirth);
            student.setDateOfGraduation(dateofgraduation);
            student.setGPA(gpa);
            // This line of code is only executed if the try block succeeds
            System.out.println(student.toString());
        } catch (InvalidDataException exp) {
            System.out.println("Test #" + testNumber + ":" + exp.getMessage());
        }
    }
    
    /**
     * This test method will try to create a student. If one of the data model
     * class set methods fails and throws an exception, the exception is caught
     * by the test method and the error is printed out. If the try block in the
     * test method succeeds, a student is created and the student
     * information is printed to the console.
     *
     */
    private static void testCreateStudent2(int testNumber, Student student, 
            String name, String address, String studentid, LocalDate dateofbirth,
            LocalDate dateofgraduation, float gpa) {
        
        try {
            student.setName(name);
            student.setAddress(address);
            student.setStudentID(studentid);
            student.setDateOfBirth(dateofbirth);
            student.setDateOfGraduation(dateofgraduation);
            student.setGPA(gpa);
            // This line of code is only executed if the try block succeeds
            System.out.println(student.toString());
        } catch (InvalidDataException exp) {
            System.out.println("Test #" + testNumber + ":" + exp.getMessage());
        }
    }


    /**
     * This test method will try to create a course. If one of the data model
     * class set methods fails and throws an exception, the exception is caught
     * by the test method and the error is printed out. If the try block in the
     * test method succeeds, a course is created and the course
     * information is printed to the console.
     *
     */
    private static void testCreateCourse(int testNumber, Course course, Classroom classroom, String courseID,
            String courseName) {
        
        try {
            course.setCourseID(courseID);
            course.setCourseName(courseName);
            course.setClassroom(classroom);
            // This line of code is only executed if the try block succeeds
            System.out.println(classroom.toString());
        } catch (InvalidDataException exp) {
            System.out.println("Test #" + testNumber + ":" + exp.getMessage() + " " + courseID);
        }
    }
    
}
