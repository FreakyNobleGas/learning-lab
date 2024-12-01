package datamodels;

import exceptionhandlers.InvalidDataException;
import java.io.Serializable;

import controllers.Application;

public class Course  implements Serializable {

    private String courseID;
    private String courseName;
    private Classroom classroom = new Classroom();

    public Course() {
    	Application.getDEBUG_LOGGER().finest("Creating Course");
    }
    
    
    public String getCourseID() {
        return courseID;
    }

    /**
     * This method will set a course ID only if it meets the course ID
     * requirements
     * 
     * @param p_courseID
     * @throws InvalidDataException 
     */
    public void setCourseID(String p_courseID) throws InvalidDataException {
        if (p_courseID.length() <= 0) {
        	Application.getDEBUG_LOGGER().finest("Course ID missing, Course not created");
            throw new InvalidDataException("Course ID missing");
        }

        if (!p_courseID.matches("^[a-zA-Z]{4}[0-9]{3}$")) {
        	Application.getDEBUG_LOGGER().finest("Invalid Course ID, Course not created");
            throw new InvalidDataException("Invalid course ID");
        }
        // If valid, set course id
        courseID = p_courseID;
        
        Application.getDEBUG_LOGGER().finest("Setting Course ID: " + courseID);
    }

    public String getCourseName() {
        return courseName;
    }

    public void setCourseName(String p_courseName) {
        this.courseName = p_courseName;
    	Application.getDEBUG_LOGGER().finest("Setting Course Name: " + this.courseName);
    }

    public Classroom getClassroom() {
        return classroom;
    }

    /**
     * This method will set a temporary default classroom if p_classroom is null
     * Note that a default classroom won't contain valid data because we aren't 
     * calling the set methods for classroom so the default data won't meet
     * the requirements for classroom data
     * 
     * @param p_classroom 
     */
    public void setClassroom(Classroom p_classroom) {
        if (p_classroom == null) {
            this.classroom = new Classroom();
        } else {
            this.classroom = p_classroom;
        }
        
    	Application.getDEBUG_LOGGER().finest("Creating classroom");
    }

    @Override
    public String toString() {
        return "Course{" + "courseID=" + courseID + ", courseName="
                + courseName + ", classroom=" + classroom + '}';
    }

}
