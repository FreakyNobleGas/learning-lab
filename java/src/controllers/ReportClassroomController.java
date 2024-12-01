/*
 * Listens for events on the report form. 
 * Implements the ActionListener interface which contains a single method, 
 * "actionPerformed"
 *
 * Handles all events on the classroom report form
 *
 * Populates the form with data if there is any in the application data model
 */
package controllers;

import datamodels.*;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;
import datacontainers.*;
import view.reportforms.ClassroomReportForm;

public class ReportClassroomController implements ActionListener {

    // The form is created here
    ClassroomReportForm form = new ClassroomReportForm();

    // Constructor 
    public ReportClassroomController(ClassroomDC applicationClassroomModel) {

        // Link this controller to the components on the form  
        // In this case, there is only one controllable component on the form, the close button
        this.form.getCloseButton().addActionListener(this);

        // Call private method that will add all classrooms to the table on the form
        this.populateTable(applicationClassroomModel);

        // make the form visible
        form.setVisible(true);

    }

    /**
     * Implements the ActionListener controller actions 
     * In this case, there is just one control
     * on the form, a close button
     */
    public void actionPerformed(java.awt.event.ActionEvent event) {

        //  Figure out which button was clicked
        String buttonClicked = event.getActionCommand();

        if (buttonClicked.equals("Close Report")) {
            form.dispose();
        }
    }

    /**
     * Private method that will add all classrooms to the table in the report form
     */
    private void populateTable(ClassroomDC p_dataModel) {

        // A table model will hold the data for the JTable (this is the M in MVC)
        DefaultTableModel tableModel = new DefaultTableModel();

        // add columns to table model
        tableModel.addColumn("Room Number");
        tableModel.addColumn("Room Type");
        tableModel.addColumn("Room Capacity");

        // Link the data model to the table
        this.form.getTableOfClassrooms().setModel(tableModel);

        // Add the classrooms in the application data model to the table model
        // The table model is linked to the table widget on the form
        for (Classroom aClassroom : p_dataModel.getListOfClassrooms()) {

            // Create a vector object that is one row in the table
            Vector aClassroomForTable = new Vector();

            // Add the data to the vector
            aClassroomForTable.add(aClassroom.getRoomNumber());
            aClassroomForTable.add(aClassroom.getTypeOfRoom());
            aClassroomForTable.add(aClassroom.getCapacity());

            // Add the row to the  data model that is connected to the JTable
            tableModel.addRow(aClassroomForTable);

        }
    }
}
