// Name: Nicholas Quinn
//
// Assignment 2

package testclasses;

import datamodels.Classroom;
import datamodels.Course;
import datamodels.Faculty;
import datamodels.Student;
import exceptionhandlers.InvalidDataException;

import java.time.LocalDate;
import java.time.Month;

public class TestAllClassesCreation {

    public static void main(String[] args) {
        
        try {
			// Create a date which will be used in all test methods
			LocalDate testDate = LocalDate.of(2018, Month.MAY, 4);
			
			// Create a classroom 
			Classroom classroom1 = new Classroom();
			classroom1.setRoomNumber("HOTH218");
			classroom1.setTypeOfRoom("Classroom");
			classroom1.setCapacity(30);
			System.out.println(classroom1.toString());

			// Create a classroom 
			Classroom classroom2 = new Classroom();
			classroom2.setRoomNumber("TATO1138");
			classroom2.setTypeOfRoom("Lab");
			classroom2.setCapacity(10);
			System.out.println(classroom2.toString());
			
			// Create a course1
			Course course1 = new Course();
			course1.setCourseID("JEDI297");
			course1.setCourseName("Deflecting Blaster Bolts, Tips & Techniques");
			course1.setClassroom(classroom1);
			System.out.println(course1.toString());

			// Create a course1
			Course course2 = new Course();
			course2.setCourseID("JEDI301");
			course2.setCourseName("Using Jedi Mind Tricks");
			course2.setClassroom(classroom2);
			System.out.println(course2.toString());
			
			// Create a Faculty member
			Faculty faculty = new Faculty();
			faculty.setDateOfBirth(testDate);
			faculty.setName("Obi-Wan Kenobi");
			faculty.setAddress("Stewjon");
			faculty.setStatus("FT");
			faculty.setDateOfHire(testDate);
			faculty.setSalary(100000000);
			faculty.getListOfCourses().add(course2);
			System.out.println(faculty.toString());

			// Create a Student 
			Student newStudent = new Student();
			newStudent.setDateOfBirth(testDate);
			newStudent.setName("Luke Skywalker");
			newStudent.setAddress("Tatooine");
			newStudent.setStudentID("000327");
			newStudent.setDateOfGraduation(testDate);
			newStudent.setGPA(2.5f);
			newStudent.getListOfCourses().add(course2);
			System.out.println(newStudent.toString());
			
			// Create another Student 
			Student newStudent2 = new Student();
			newStudent2.setDateOfBirth(testDate);
			newStudent2.setName("Anakine Skywalker");
			newStudent2.setAddress("Tatooine");
			newStudent2.setStudentID(512);
			newStudent2.setDateOfGraduation(testDate);
			newStudent2.setGPA(2.5f);
			newStudent2.getListOfCourses().add(course2);
			System.out.println(newStudent2.toString());
			
		} catch (InvalidDataException e) {
			e.printStackTrace();
		}

    }
    
    
}
