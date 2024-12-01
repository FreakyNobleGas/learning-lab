package view.reportforms;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;

/**
 * This class creates a classroom report form. 
 */
public class ClassroomReportForm extends JFrame  {

    // Form controls
    
    /** button to close the form */
    private JButton closeButton;
    /** label on form containing form title */
    private JLabel formTitle;
    /** scrollable panel for the table */
    private JScrollPane tableScrollPanel;
    /** table to display data from the data model */
    private JTable tableOfClassrooms;

    /**
     * Constructor to create the form
     */
    public ClassroomReportForm() {        
      
        /** 
         * Form components
         */
        formTitle = new JLabel("Tabular List of Classrooms");
        closeButton = new JButton("Close Report");
        tableScrollPanel = new JScrollPane();
        tableOfClassrooms = new JTable();

        /** set close function to dispose of the form */
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        
        /** set the form layout to null, we'll place the components manually 
         *  using x,y layout positions
         */
        getContentPane().setLayout(null);

        // Form title
        /** Add title to the form */
        getContentPane().add(formTitle);
        /** location and size of component on the form */
        formTitle.setBounds(300, 30, 300, 14);

        // Close button
        /** Add button to the form */
        getContentPane().add(closeButton);
        /** location and size of component on the form */
        closeButton.setBounds(320, 220, 120, 23);

        // Table component
        /** Put the table in the scrollable panel */
        tableScrollPanel.setViewportView(tableOfClassrooms);        
        /** Add scrollable panel to the form */
        getContentPane().add(tableScrollPanel);
        /** location and size of scrollable panel on the form */
        tableScrollPanel.setBounds(20, 60, 720, 138);

        /** set the overall size of the form */
        setSize(800, 350);
    }

    // Getters that allow access to the controls on the form via the controller

    public JButton getCloseButton() {
        return this.closeButton;
    }

    public JTable getTableOfClassrooms() {
        return tableOfClassrooms;
    }  
   
}
