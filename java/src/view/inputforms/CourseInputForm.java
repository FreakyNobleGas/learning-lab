/**
 * This class is used to create the offered course input form
 */

package view.inputforms;

import controllers.InputCourseFormController;
import datamodels.*;
import java.util.ArrayList;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import datacontainers.*;

public class CourseInputForm extends javax.swing.JFrame {

    private JTextField courseIdField = new JTextField();
    private JTextField courseNameField = new JTextField();
    private JButton exitButton = new JButton("Close");
    private JButton clearButton = new JButton("Clear");
    private JButton saveButton = new JButton("Save");
    private JLabel formTitle = new JLabel("Add A Course");
    private JLabel courseNumberFieldLabel = new JLabel("Course Number:");
    private JLabel courseNameFieldLabel = new JLabel("Course Name:");
    private JLabel classroomListLabel = new JLabel("Select a Classroom of Available Classrooms");
    private JScrollPane listOfClassroomsScrollablePanel = new JScrollPane();
    private JList dropdownClassroomList = new JList();
    
    private DefaultListModel dropdownListDataModel = new DefaultListModel();

    public CourseInputForm(InputCourseFormController controller) {
       
       setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        getContentPane().setLayout(null);

        getContentPane().add(formTitle);
        formTitle.setBounds(220, 10, 150, 14);

        getContentPane().add(courseIdField);
        courseIdField.setBounds(160, 50, 200, 30);

        getContentPane().add(courseNameField);
        courseNameField.setBounds(160, 90, 200, 30);

        getContentPane().add(courseNumberFieldLabel);
        courseNumberFieldLabel.setBounds(10, 50, 130, 14);

        getContentPane().add(courseNameFieldLabel);
        courseNameFieldLabel.setBounds(10, 90, 140, 14);

        getContentPane().add(classroomListLabel);
        classroomListLabel.setBounds(10, 150, 260, 14);

        getContentPane().add(saveButton);
        saveButton.setBounds(400, 50, 80, 23);

        getContentPane().add(clearButton);
        clearButton.setBounds(400, 80, 80, 23);

        getContentPane().add(exitButton);
        exitButton.setBounds(400, 110, 80, 23);

        listOfClassroomsScrollablePanel.setViewportView(dropdownClassroomList);

        getContentPane().add(listOfClassroomsScrollablePanel);
        listOfClassroomsScrollablePanel.setBounds(10, 180, 450, 170);

        ClassroomDC listOfClassesDataModel = controller.getClassroomDC();
        ArrayList<Classroom> listOfClassrooms = listOfClassesDataModel.getListOfClassrooms();
        
        for (int i = 0; i < listOfClassrooms.size(); i++) {
            dropdownListDataModel.addElement(listOfClassrooms.get(i));
        }
        
        // Link the data model to the list on the form
        dropdownClassroomList.setModel(dropdownListDataModel);


        // Link this controller to the buttons on the form
        this.clearButton.addActionListener(controller);
        this.saveButton.addActionListener(controller);
        this.exitButton.addActionListener(controller);

        // Set the size of the form
        this.setSize(600, 400);

    }

    // Accessor methods used by the controller class to retrieve the data from
    // the form

    public JTextField getCourseIdField() {
        return courseIdField;
    }

    public JTextField getCourseNameField() {
        return courseNameField;
    }

    public JList getListOfClassrooms() {
        return dropdownClassroomList;
    }
    
    
}
