package datamodels;

import exceptionhandlers.InvalidDataException;
import java.time.LocalDate;
import java.util.ArrayList;

import controllers.Application;

public class Faculty extends Person {

    private LocalDate dateOfHire;
    private double salary;
    private String status;
    private ArrayList<Course> listOfCourses = new ArrayList<>();

    public Faculty() {
    	Application.getDEBUG_LOGGER().finest("Creating Faculty");
    }
    
    public LocalDate getDateOfHire() {
        return dateOfHire;
    }

    public void setDateOfHire(LocalDate p_dateOfHire) {
        dateOfHire = p_dateOfHire;
        Application.getDEBUG_LOGGER().finest("Setting Date Of Hire: " + dateOfHire);
    }

    public double getSalary() {
        return salary;
    }

    public void setSalary(double p_salary) throws InvalidDataException {
        if (p_salary <= 0) {
            salary = 0;
            Application.getDEBUG_LOGGER().finest("Invalid Salary, setting to $0.");
            throw new InvalidDataException("Invalid Salary, setting to $0");
        } else {
            salary = p_salary;
        };
        
        Application.getDEBUG_LOGGER().finest("Setting salary of " + salary);
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String p_status) throws InvalidDataException{
        if ((p_status == null) || (p_status.length() == 0)) {
            status = "Full Time";
            Application.getDEBUG_LOGGER().finest("Invalid Status, setting to full time");
            throw new InvalidDataException("Invalid status, setting to full time");
        } else {
            status = p_status;
        }
        
        Application.getDEBUG_LOGGER().finest("Setting Faculty status to " + status);
    }

    public ArrayList<Course> getListOfCourses() {
        return listOfCourses;
    }

    @Override
    public String toString() {
        return "Faculty{name=" + super.getName() + ", address=" + super.getAddress()
                + ", dateOfBirth=" + super.getDateOfBirth() + ", dateOfHire="
                + dateOfHire + ", salary=" + salary + ", status="
                + status + ", listOfCourses=" + listOfCourses + '}';
    }
}