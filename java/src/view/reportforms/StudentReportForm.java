package view.reportforms;

import datamodels.Student;
import controllers.ReportStudentController;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.WindowConstants;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;

/**
 * This class contains all the code for the student report form. It creates the
 * form and populates the form with the data from the student data model
 */
public class StudentReportForm extends JFrame {

    // Form components
    
    private JButton closeButton = new JButton("Close Report");
    private JScrollPane treeScrollPane;
    private JTree studentTree;
    private DefaultMutableTreeNode topLevelNode;
    private JTextArea courseData;

    public StudentReportForm(ReportStudentController controller) {

        // Closes the form by default
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        // Add to form in the bottom of the panel
        getContentPane().add(closeButton, BorderLayout.SOUTH);
        topLevelNode = new DefaultMutableTreeNode("Students");

        // Create the tree with the top level node
        studentTree = new JTree(topLevelNode);
        // Create a scroll panel for the tree
        treeScrollPane = new JScrollPane(studentTree);
        // Allow only one tree selection at a time
        studentTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        //Listen for when the selection changes.
        studentTree.addTreeSelectionListener(controller);

        // Set up the text area for course data
        courseData = new JTextArea();

        //Add the tree pane and the course detail pain to a split pane.
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        splitPane.setTopComponent(treeScrollPane);
        splitPane.setBottomComponent(courseData);

        // Set the default split location and size of the pane
        splitPane.setDividerLocation(175);
        splitPane.setPreferredSize(new Dimension(500, 300));

        // Add the split pane to the form
        getContentPane().add(splitPane, BorderLayout.CENTER);

        // Once the form is initialized, retrieve the data from the passed in 
        // data model
        ArrayList<Student> listOfStudents = controller.getDC().getListOfStudents();
                
        // Call private method that will add all students to the tree
        this.populateTree(listOfStudents);

        // Link the controller to the button
        closeButton.addActionListener(controller);

        // Set the size of the form
        setSize(800, 350);

    }

    private void populateTree(ArrayList<Student> listOfStudents) {

        // Add all students to the tree
        for (Student astudent : listOfStudents) {

            // Top level node for each student is the name field
            DefaultMutableTreeNode aStudent = new DefaultMutableTreeNode(astudent.getName());

            // All the other meta data fields are child nodes of the name field
            DefaultMutableTreeNode address = new DefaultMutableTreeNode(astudent.getAddress());
            DefaultMutableTreeNode ssn = new DefaultMutableTreeNode(astudent.getStudentID());
            DefaultMutableTreeNode dob = new DefaultMutableTreeNode(astudent.getDateOfBirth());
            DefaultMutableTreeNode dog = new DefaultMutableTreeNode(astudent.getDateOfGraduation());
            DefaultMutableTreeNode gpa = new DefaultMutableTreeNode(astudent.getGPA()+ "");

            aStudent.add(address);
            aStudent.add(ssn);
            aStudent.add(dob);
            aStudent.add(dog);
            aStudent.add(gpa);

            // Add the student node to the tree
            topLevelNode.add(aStudent);
        }
    }

    // Getters to access the controls on the form
    public JButton getCloseButton() {
        return closeButton;
    }

    public JTree getStudentTree() {
        return studentTree;
    }

    public DefaultMutableTreeNode getTopLevelNode() {
        return topLevelNode;
    }

}
