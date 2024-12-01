/*
 * Listens for events on the report form. 
 * Implements the ActionListener interface which contains a single method, 
 * "actionPerformed"
 *
 * Populates the form with data if there is any in the application data model
 */
package controllers;

import java.awt.event.ActionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import datacontainers.StudentDC;
import view.reportforms.StudentReportForm;

public class ReportStudentController implements ActionListener, TreeSelectionListener {

    private StudentReportForm form;
    private StudentDC DC;
    
    // A table model will hold the data for the JTable (this is the M in MVC)
    private DefaultTableModel defaultTableModel = new DefaultTableModel();

    public ReportStudentController(StudentDC studentDC) {
        this.DC = studentDC;
        form = new StudentReportForm(this);
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
     * Implements the valueChanged method in the TreeSelectionListener interface. 
     */
    public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)
                           this.form.getStudentTree().getLastSelectedPathComponent();

        if (node == null) return;

        Object nodeInfo = node.getUserObject();
        if (node.isLeaf()) {
            // Display course info
        }
    }

    // Getters used by the controller
    
    public StudentReportForm getForm() {
        return form;
    }

    public StudentDC getDC() {
        return DC;
    }


}
