package view.reportforms;

import datamodels.Faculty;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

/**
 * This class contains all the code for the faculty report form
 */
public class FacultyReportForm extends JFrame  {

    // Form controls
    
    /** button to close the form */
    private JButton closeButton = new JButton("Close Report");
    
    /** label for free text area on form */
    private JLabel label1 = new JLabel("List Of Faculty At A Glance");
    
    // Title of form
    private JLabel label2 = new JLabel("List of Faculty");
    
    /** scrollable panels for the table and free form text area */
    private JScrollPane textAreaScrollPanel = new JScrollPane();
    private JScrollPane tableScrollPanel = new JScrollPane();
    
     /** table to display data from the data model */
    private JTable tableOfFaculty = new javax.swing.JTable();
    
    /** Free form text area */
    private JTextArea textAreaOfFaculty = new JTextArea(20, 5);
    
    /**
     * Constructor to create the form
     */
    public FacultyReportForm() {        
        
        /** set close function to dispose of the form */
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
       
        /** set the form layout to null, we'll place the components manually 
         *  using x,y layout positions
         */
        getContentPane().setLayout(null);

        /** Add label component to the form */
        getContentPane().add(label1);
        label1.setBounds(300, 20, 210, 14);

         /** Put the table in the scrollable panel */
        textAreaScrollPanel.setViewportView(textAreaOfFaculty);

         /** Add component to the form */
        getContentPane().add(textAreaScrollPanel);
        textAreaScrollPanel.setBounds(10, 45, 720, 124);

         /** Add label component to the form */
        getContentPane().add(label2);
        label2.setBounds(320, 190, 270, 14);

        /** Add component to the form */
        getContentPane().add(closeButton);
        closeButton.setBounds(300, 380, 120, 23);
        
        tableOfFaculty.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null}
            },
            new String [] {
                "Name", "Address", "Status", "Salary", "Date of Birth", "Date of Hire"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.Float.class, java.lang.String.class, java.lang.String.class
            };
            boolean[] canEdit = new boolean [] {
                false, false, false, false, false, false
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        tableOfFaculty.setColumnSelectionAllowed(true);
        // Put the table in a scrollablepanel
        tableScrollPanel.setViewportView(tableOfFaculty);
        
        // Add the panel to the form
        getContentPane().add(tableScrollPanel);
        tableScrollPanel.setBounds(10, 220, 720, 138);
        
        /** set the overall size of the form */
        setSize(800, 550);
    }
   
      /**
     * Private method that will add all Faculty to the text area
     */
    private void populateTextArea(ArrayList<Faculty> listOfFaculty) {

        // Initialize the string which will hold the data for the text area
        // Start with labels
        String listOfFacultyString = "";

        // Loop through the list and add the faculty names to the text area,
        // Each time adding a cr/lf between items for readibility.
        for (Faculty afaculty : listOfFaculty) {
            listOfFacultyString += afaculty.getName() + "\n";
        }
        // Once all the data is in the string, set the text of the text area to the string value
        textAreaOfFaculty.setText(listOfFacultyString);

    }
  
    // Getters that allow access to the controls on the form
    
    public JButton getCloseButton() {
        return closeButton;
    }

    public JTable getTableOfFaculty() {
        return tableOfFaculty;
    }

    public JTextArea getTextAreaOfFaculty() {
        return textAreaOfFaculty;
    }


}
