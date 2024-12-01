/*
 * Listens for events on the report form. 
 * Implements the ActionListener interface which contains a single method, 
 * "actionPerformed"
 *
 * Populates the form with data if there is any in the application data model
 */
package controllers;

import datamodels.*;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;
import datacontainers.*;
import view.reportforms.FacultyReportForm;

public class ReportFacultyController implements ActionListener {

    // The form
    FacultyReportForm form;

    public ReportFacultyController(FacultyDC facultyDataModel) {

        // Create the form
        form = new FacultyReportForm();

        // Link the buttons to the actionPerformed method
        this.form.getCloseButton().addActionListener(this);

        // Call private method that will add all Faculty to the text area
        this.populateTextArea(facultyDataModel);

        // Call private method that will add all Faculty to the table
        this.populateTable(facultyDataModel);

        // make the form visible
        form.setVisible(true);
    }

    /**
     * Implements actionPerformed method of the ActionListener interface
     */
    public void actionPerformed(java.awt.event.ActionEvent event) {

         //  Figure out which button was clicked
        String buttonClicked = event.getActionCommand();

        if (buttonClicked.equals("Close Report")) {
            form.dispose();
        }
    }

    /**
     * Private method that will add all Faculty to the text area
     */
    private void populateTextArea(FacultyDC facultyDataModel) {

        // Initialize the string which will hold the data for the text area
        // Start with labels
        String listOfFacultyString = "";

        // Loop through the list and add the faculty names to the text area,
        // Each time adding a cr/lf between items for readibility.
        for (Faculty afaculty : facultyDataModel.getListOfFaculty()) {
            listOfFacultyString += afaculty.getName() + "\n";
        }
        // Once all the data is in the string, set the text of the text area to the string value
        this.form.getTextAreaOfFaculty().setText(listOfFacultyString);

    }

    /**
     * Private method that will add all Faculty to the table
     */
    private void populateTable(FacultyDC applicationFacultyDC) {

        // A table model like this will hold the data for the JTable (this is the M in MVC)
        DefaultTableModel tableModel = new DefaultTableModel();

        // Link the data model to the table
        this.form.getTableOfFaculty().setModel(tableModel);

        // Add columns to table model
        tableModel.addColumn("Name");
        tableModel.addColumn("Address");
        tableModel.addColumn("Status");
        tableModel.addColumn("Salary");
        tableModel.addColumn("Date Of Birth");
        tableModel.addColumn("Date Of Hire");
        
        // Add the Faculty from the application data model to the table data model
        for (Faculty afaculty : applicationFacultyDC.getListOfFaculty()) {

            // Create a vector that is one row in the table
            Vector tableRow = new Vector();

            // Add the data to the vector
            tableRow.add(afaculty.getName());
            tableRow.add(afaculty.getAddress());
            tableRow.add(afaculty.getStatus());
            tableRow.add(afaculty.getSalary());
            tableRow.add(afaculty.getDateOfBirth());
            tableRow.add(afaculty.getDateOfHire());
            
            // Format the course data
            String courselist = afaculty.getListOfCourses().toString();

            tableRow.add(courselist);

            // Add the row of data to the  data model that is connected to the JTable
            tableModel.addRow(tableRow);
        }
    }

}
