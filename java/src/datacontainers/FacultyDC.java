package datacontainers;

import datamodels.Faculty;
import java.util.ArrayList;

public class FacultyDC {

    private ArrayList<Faculty> listOfFaculty = new ArrayList<>();

    public FacultyDC(){
    }

    public ArrayList<Faculty> getListOfFaculty() {
        return listOfFaculty;
    }

    public void setListOfFaculty(ArrayList<Faculty> listOfFaculty) {
        this.listOfFaculty = listOfFaculty;
    }
}
