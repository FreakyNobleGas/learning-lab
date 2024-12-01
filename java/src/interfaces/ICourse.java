package interfaces;

import datamodels.Classroom;
import exceptionhandlers.InvalidDataException;

public interface ICourse {

	public String getCourseID();
	public String getCourseName();
	public Classroom getClassroom();
	public void setCourseID(String courseID) throws InvalidDataException;
	public void setCourseName(String courseName);
	public void setClassroom(Classroom classroom);
	
}
