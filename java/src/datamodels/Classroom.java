package datamodels;

import controllers.Application;
import exceptionhandlers.InvalidDataException;
import interfaces.IClassroom;
import java.io.Serializable;

public class Classroom  implements IClassroom, Serializable {

    private String roomNumber = "XXX";
    private String typeOfRoom = "UNKNOWN";
    private int capacity;

    // Add a constructor that will log an audit message when a classroom is created
    public Classroom() {
        Application.getDEBUG_LOGGER().finest("Creating classroom");
    }   
    
    public void setRoomNumber(String p_roomNumber) throws InvalidDataException {
        // Test for valid room number
        if (p_roomNumber.length() == 0) {
            Application.getDEBUG_LOGGER().finest("Invalid room number, classroom not created");
            throw new InvalidDataException("No room number specified");
        }
        if (!p_roomNumber.matches("^[a-zA-Z]{2}[0-9]{3}$")) {
            Application.getDEBUG_LOGGER().finest("Invalid room number, classroom not created");
            throw new InvalidDataException("Invalid room number");
        }
        // If valid, set room number
        this.roomNumber = p_roomNumber;
        
        Application.getDEBUG_LOGGER().finest("Setting room number: " + roomNumber);
    }

    public void setTypeOfRoom(String p_typeOfRoom) throws InvalidDataException {
        // Test for empty string
        if (p_typeOfRoom.length() == 0) {
            Application.getDEBUG_LOGGER().finest("Invalid room type, classroom not created");
            throw new InvalidDataException("No room type specified");
        }
        // Test for invalid string
        if ((!p_typeOfRoom.equals("LAB")) && (!p_typeOfRoom.equals("CLASSROOM"))
                && (!p_typeOfRoom.equals("LECTURE HALL"))) {
            Application.getDEBUG_LOGGER().finest("Invalid room type, classroom not created");
            throw new InvalidDataException("Invalid room type specified");
        }
        // If valid, set room type
        typeOfRoom = p_typeOfRoom;
        
        Application.getDEBUG_LOGGER().finest("Setting type of room: " + typeOfRoom);
    }

    public void setCapacity(int p_capacity) throws InvalidDataException {

        // Test for valid value 
        if (p_capacity <= 0) {
            Application.getDEBUG_LOGGER().finest("Invalid capacity, classroom not created");
            throw new InvalidDataException("Room capacity must be greater than 0");
        }
        // If valid, set room capacity
        capacity = p_capacity;

        Application.getDEBUG_LOGGER().finest("Setting room capacity: " + capacity);
    }
    
    public void setCapacity(String p_capacity) throws InvalidDataException {

        // Test for valid value 
        try {
            Integer.parseInt(p_capacity);
        } catch (Exception exp) {
            Application.getDEBUG_LOGGER().finest("Invalid capacity, classroom not created");
            throw new InvalidDataException("Room capacity is not a valid value");
        }

        setCapacity(p_capacity);
        
        Application.getDEBUG_LOGGER().finest("Setting room capacity: " + capacity);
    }

    public String getRoomNumber() {
        return roomNumber;
    }

    public String getTypeOfRoom() {
        return typeOfRoom;
    }

    public int getCapacity() {
        return capacity;
    }

    @Override
    public String toString() {
        return "Classroom{" + "roomNumber=" + roomNumber + ", typeOfRoom="
                + typeOfRoom + ", capacity=" + capacity + '}';
    }

}
