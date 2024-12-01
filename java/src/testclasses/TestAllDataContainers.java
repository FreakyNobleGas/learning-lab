package testclasses;

import datacontainers.ClassroomDC;
import datacontainers.CourseDC;
import datacontainers.FacultyDC;
import datacontainers.StudentDC;
import datamodels.Classroom;
import datamodels.Course;
import datamodels.Faculty;
import datamodels.Student;
import exceptionhandlers.InvalidDataException;

import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;

public class TestAllDataContainers {

    public static void main(String[] args) {

        try {
			// Create a date which will be used in all test methods
			LocalDate testDate = LocalDate.of(2018, Month.MAY, 4);

			// Create data containers
			ClassroomDC classroomDataContainer = new ClassroomDC();
			CourseDC courseDataContainer = new CourseDC();
			FacultyDC facultyDataContainer = new FacultyDC();
			StudentDC studentDataContainer = new StudentDC();

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

			// Create a course
			Course course1 = new Course();
			course1.setCourseID("JEDI301");
			course1.setCourseName("Deflecting Blaster Bolts, Tips & Techniques");
			course1.setClassroom(classroom1);
			// Add to data container
			courseDataContainer.getListOfCourses().add(course1);

			// Create a course
			Course course2 = new Course();
			course2.setCourseID("JEDI301");
			course2.setCourseName("Deflecting Blaster Bolts, Tips & Techniques");
			course2.setClassroom(classroom2);
			// Add to data container
			courseDataContainer.getListOfCourses().add(course2);

			// Create a course
			Course course3 = new Course();
			course3.setCourseID("JEDI301");
			course3.setCourseName("Deflecting Blaster Bolts, Tips & Techniques");
			course3.setClassroom(classroom3);
			// Add to data container
			courseDataContainer.getListOfCourses().add(course3);

			// Print out all the objects in the data container
			ArrayList<Course> listOfCCoursesInContainer = courseDataContainer.getListOfCourses();

			for (Course oneCourse : listOfCCoursesInContainer) {
			    System.out.println(oneCourse.toString());
			}

			// Create a Faculty member
			Faculty faculty1 = new Faculty();
			faculty1.setDateOfBirth(testDate);
			faculty1.setName("Obi-Wan Kenobi");
			faculty1.setAddress("Stewjon");
			faculty1.setStatus("FT");
			faculty1.setDateOfHire(testDate);
			faculty1.setSalary(100000000);
			faculty1.getListOfCourses().add(course1);
			
			// Add to data container
			facultyDataContainer.getListOfFaculty().add(faculty1);

			// Create a Faculty member
			Faculty faculty2 = new Faculty();
			faculty2.setDateOfBirth(testDate);
			faculty2.setName("Obi-Wan Kenobi");
			faculty2.setAddress("Stewjon");
			faculty2.setStatus("FT");
			faculty2.setDateOfHire(testDate);
			faculty2.setSalary(100000000);
			faculty2.getListOfCourses().add(course2);
			
			// Add to data container
			facultyDataContainer.getListOfFaculty().add(faculty2);

			// Print out all the objects in the data container
			ArrayList<Faculty> listOfFacultyInContainer = facultyDataContainer.getListOfFaculty();

			for (Faculty oneCourse : listOfFacultyInContainer) {
			    System.out.println(oneCourse.toString());
			}

			// Create a Student 
			Student newStudent1 = new Student();
			newStudent1.setDateOfBirth(testDate);
			newStudent1.setName("Hans Solo");
			newStudent1.setAddress("Tatooine");
			newStudent1.setStudentID("009999");
			newStudent1.setDateOfGraduation(testDate);
			newStudent1.setGPA(1.5f);
			newStudent1.getListOfCourses().add(course1);
			
			// Add to data container
			studentDataContainer.getListOfStudents().add(newStudent1);

			// Create another Student 
			Student newStudent2 = new Student();
			newStudent2.setDateOfBirth(testDate);
			newStudent2.setName("Leah Skywalker");
			newStudent2.setAddress("Tatooine");
			newStudent2.setStudentID(411);
			newStudent2.setDateOfGraduation(testDate);
			newStudent2.setGPA(5.5f);
			newStudent2.getListOfCourses().add(course2);

			 // Add to data container
			studentDataContainer.getListOfStudents().add(newStudent2);
			
			// Print out all the objects in the data container
			ArrayList<Student> listOfStudentsInContainer = studentDataContainer.getListOfStudents();

			for (Student oneStudent : listOfStudentsInContainer) {
			    System.out.println(oneStudent.toString());
			}
		} catch (InvalidDataException e) {
			e.printStackTrace();
		}
    }
}