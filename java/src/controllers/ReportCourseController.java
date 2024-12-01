/*
 * Listens for events on the report form. 
 * Implements the ActionListener interface which contains a single method, 
 * "actionPerformed"
 *
 * Populates the form with data if there is any in the application data model
 */
package controllers;

import datamodels.Course;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;
import datacontainers.CourseDC;
import view.reportforms.CourseReportForm;

public class ReportCourseController implements ActionListener {

    // The form is created here
    CourseReportForm form = new CourseReportForm();

    // Constructor 
    public ReportCourseController(CourseDC CourseDC) {

         // Link the buttons to the actionPerformed method
        this.form.getCloseButton().addActionListener(this);
        
        // Call private method that will add all courses to the text area
        this.populateTextArea(CourseDC);

        // Call private method that will add all courses to the table
        this.populateTable(CourseDC);

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
     * Private method that will add all courses to the text area
     */
    private void populateTextArea(CourseDC CourseDC) {

        // Initialize the string which will hold the data for the text area
        // Start with labels
        String allCoursesText = "";

        // Loop through the list and add the courses to the text area,
        // Each time adding a cr/lf between items for readibility.
        for (Course aCourse : CourseDC.getListOfCourses()) {
            String courseId = aCourse.getCourseID();
            String courseName = aCourse.getCourseName();
            allCoursesText += courseId;
            allCoursesText += " ";
            allCoursesText += courseName;
            allCoursesText += "\n";
        }
        // Once all the data is in the string, set the text of the text area to the string value
        this.form.getTextAreaOfAvailableCourses().setText(allCoursesText);

    }

    /**
     * Private method that will add all courses to the table
     */
    private void populateTable(CourseDC CourseDC) {

        // This data model will hold the data for the JTable (this is the M in MVC)
        DefaultTableModel defaultTableModel = new DefaultTableModel();

        // Because it's a table, we have to add columns to table model to describe how 
        // the data is layed out
        defaultTableModel.addColumn("Course Number");
        defaultTableModel.addColumn("Course Name");
        defaultTableModel.addColumn("Classroom");

        // Link the data model to the table
        this.form.getTableOfAvailableCourses().setModel(defaultTableModel);
 
        // Add the courses to the table model
        for (Course aCourse : CourseDC.getListOfCourses()) {

            // Create a vector that is one row in the table
            Vector aCourseForTable = new Vector();

            // Add the data to the vector
            aCourseForTable.add(aCourse.getCourseID());
            aCourseForTable.add(aCourse.getCourseName());
            aCourseForTable.add(aCourse.getClassroom().getRoomNumber());

            // Add the vector to the  data model that is connected to the JTable
            defaultTableModel.addRow(aCourseForTable);
        }
    }

}
