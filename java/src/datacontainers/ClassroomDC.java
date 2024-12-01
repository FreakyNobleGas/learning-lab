/*
 *  This Class contains a list which will hold classroom objects created in the UI
 */
package datacontainers;

import datamodels.Classroom;

import java.util.ArrayList;

public class ClassroomDC {

    private ArrayList<Classroom> listOfClassrooms = new ArrayList<>();

    /**
     * no-arg constructor
     */
    public ClassroomDC() {
    }

    public ArrayList<Classroom> getListOfClassrooms() {
        return listOfClassrooms;
    }

    public void setListOfClassrooms(ArrayList<Classroom> listOfClassrooms) {
        this.listOfClassrooms = listOfClassrooms;
    }
    
}
