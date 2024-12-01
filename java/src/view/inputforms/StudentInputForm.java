package view.inputforms;

import controllers.InputStudentFormController;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JTextField;

public class StudentInputForm extends javax.swing.JFrame {

// Form fields

    private javax.swing.JTextField addressField;
    private javax.swing.JButton clearButton;
    private javax.swing.JButton closeButton;
    private javax.swing.JFormattedTextField dateOfBirthFormattedTextField1;
    private javax.swing.JFormattedTextField dateOfGraduationFormattedTextField;
    private javax.swing.JTextField gpaField;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JTextField nameField;
    private javax.swing.JButton saveButton;
    private javax.swing.JTextField studentIdField;


    /**
     * default constructor
     */
    public StudentInputForm(InputStudentFormController controller) {
        
        // Initialize all the components on the form
        initComponents();
        
        // Link the controller to the buttons on the form
        this.clearButton.addActionListener(controller);
        this.saveButton.addActionListener(controller);
        this.closeButton.addActionListener(controller);

        // Set the size of the form
        this.setSize(600, 400);
        
        // Make the form visible
        this.setVisible(true);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     */
    @SuppressWarnings("unchecked")    
    private void initComponents() {

        jLabel7 = new javax.swing.JLabel();
        nameField = new javax.swing.JTextField();
        addressField = new javax.swing.JTextField();
        studentIdField = new javax.swing.JTextField();
        closeButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        saveButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        jLabel11 = new javax.swing.JLabel();
        gpaField = new javax.swing.JTextField();
        dateOfGraduationFormattedTextField = new javax.swing.JFormattedTextField();
        dateOfBirthFormattedTextField1 = new javax.swing.JFormattedTextField();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();

        jLabel7.setText("jLabel7");

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().setLayout(null);
        getContentPane().add(nameField);
        nameField.setBounds(160, 50, 200, 30);
        getContentPane().add(addressField);
        addressField.setBounds(160, 90, 200, 30);
        getContentPane().add(studentIdField);
        studentIdField.setBounds(160, 130, 200, 30);

        closeButton.setText("Close");
        getContentPane().add(closeButton);
        closeButton.setBounds(390, 130, 110, 23);

        jLabel1.setText("Add Student");
        getContentPane().add(jLabel1);
        jLabel1.setBounds(220, 10, 130, 14);

        jLabel2.setText("Name:");
        getContentPane().add(jLabel2);
        jLabel2.setBounds(10, 50, 140, 14);

        jLabel3.setText("Address:");
        getContentPane().add(jLabel3);
        jLabel3.setBounds(10, 90, 140, 14);

        jLabel4.setText("Student ID");
        getContentPane().add(jLabel4);
        jLabel4.setBounds(10, 130, 140, 14);

        jLabel5.setText("Data Of Birth");
        getContentPane().add(jLabel5);
        jLabel5.setBounds(10, 200, 130, 14);

        jLabel6.setText("Data Of Graduation");
        getContentPane().add(jLabel6);
        jLabel6.setBounds(10, 240, 120, 14);

        saveButton.setText("Save");
        getContentPane().add(saveButton);
        saveButton.setBounds(390, 50, 110, 23);

        clearButton.setText("Clear");
        getContentPane().add(clearButton);
        clearButton.setBounds(390, 90, 110, 23);

        jLabel11.setText("GPA");
        getContentPane().add(jLabel11);
        jLabel11.setBounds(10, 290, 120, 14);
        getContentPane().add(gpaField);
        gpaField.setBounds(160, 290, 80, 20);

        try {
            dateOfGraduationFormattedTextField.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.MaskFormatter("##-##-####")));
        } catch (java.text.ParseException ex) {
            ex.printStackTrace();
        }
        getContentPane().add(dateOfGraduationFormattedTextField);
        dateOfGraduationFormattedTextField.setBounds(160, 230, 130, 30);

        try {
            dateOfBirthFormattedTextField1.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.MaskFormatter("##-##-####")));
        } catch (java.text.ParseException ex) {
            ex.printStackTrace();
        }
        getContentPane().add(dateOfBirthFormattedTextField1);
        dateOfBirthFormattedTextField1.setBounds(160, 190, 130, 30);

        jLabel8.setText("MM-DD-YYYY");
        getContentPane().add(jLabel8);
        jLabel8.setBounds(300, 240, 160, 14);

        jLabel9.setText("MM-DD-YYYY");
        getContentPane().add(jLabel9);
        jLabel9.setBounds(300, 200, 160, 14);

        pack();
    }

    public JTextField getAddressField() {
        return addressField;
    }

    public JButton getClearButton() {
        return clearButton;
    }

    public JFormattedTextField getDateOfBirthFormattedTextField1() {
        return dateOfBirthFormattedTextField1;
    }

    public JFormattedTextField getDateOfGraduationFormattedTextField() {
        return dateOfGraduationFormattedTextField;
    }

    public JButton getExitButton() {
        return closeButton;
    }

    public JTextField getGpaField() {
        return gpaField;
    }

    public JLabel getjLabel6() {
        return jLabel6;
    }

    public JLabel getjLabel9() {
        return jLabel9;
    }

    public JTextField getNameField() {
        return nameField;
    }

    public JButton getSaveButton() {
        return saveButton;
    }

    public JTextField getStudentIdField() {
        return studentIdField;
    }

    
    
}
