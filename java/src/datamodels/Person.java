package datamodels;

import exceptionhandlers.InvalidDataException;
import java.io.Serializable;
import java.time.LocalDate;

import controllers.Application;

public abstract class Person implements Serializable {

    private String name;
    private String address;
    private LocalDate dateOfBirth;
    
    public Person() {
    	Application.getDEBUG_LOGGER().finest("Creating Person.");
    }

    public String getName() {
        return name;
    }

    public void setName(String p_name) throws InvalidDataException {
        if (p_name.length() == 0) {
        	Application.getDEBUG_LOGGER().finest("Invalid Name, Person not created.");
            throw new InvalidDataException("Name required");
        } else {
            name = p_name;
        }
        Application.getDEBUG_LOGGER().finest("Creating person with name: " + name);
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String p_address) throws InvalidDataException {
        if (p_address.length() == 0) {
        	Application.getDEBUG_LOGGER().finest("Invalid Address, Person not created.");
            throw new InvalidDataException("Address required");
        } else {
            address = p_address;
        }
        
        Application.getDEBUG_LOGGER().finest("Person created with Address: " + address);
    }

    public java.time.LocalDate getDateOfBirth() {
        return dateOfBirth;
    }

    public void setDateOfBirth(java.time.LocalDate p_dateOfBirth) throws InvalidDataException {
        if (p_dateOfBirth == null)  {
            dateOfBirth = null;
            Application.getDEBUG_LOGGER().finest("Invalid Date of Birth. Person not created.");
            throw new InvalidDataException("Date of birth not specified, setting to null");
        } else {
            dateOfBirth = p_dateOfBirth;
        }
        
        Application.getDEBUG_LOGGER().finest("Person created with Date Of Birth: " + dateOfBirth);
    }

    @Override
    public String toString() {
        return "Person{" + "name=" + name + ", address=" + address
                + ", dateOfBirth=" + dateOfBirth + '}';
    }
}