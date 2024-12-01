package view.inputforms;

import controllers.InputClassroomFormController;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JTextField;

public class ClassroomInputForm extends javax.swing.JFrame {

    // Form components  
    /**
     * use default constructors to create the form components and then use
     * setters to to set the values when the form is layout out
     */    
    private javax.swing.JLabel roomNumberLabel;
    private javax.swing.JTextField roomNumberTextfield;
    private javax.swing.JComboBox roomTypeCombobox;
    private javax.swing.JLabel roomTypeLabel;
    private javax.swing.JLabel roomCapacityLabel;
    private javax.swing.JTextField roomCapacityTextField;
    private javax.swing.JButton saveButton;
    private javax.swing.JButton clearButton;
    private javax.swing.JButton closeButton;

    /**
     * Constructor
     */
    public ClassroomInputForm(InputClassroomFormController controller) {

        // Instantiate the components on the form
        roomNumberLabel = new javax.swing.JLabel();
        roomTypeLabel = new javax.swing.JLabel();
        roomCapacityLabel = new javax.swing.JLabel();
        roomNumberTextfield = new javax.swing.JTextField();
        roomTypeCombobox = new javax.swing.JComboBox();
        roomCapacityTextField = new javax.swing.JTextField();
        saveButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        closeButton = new javax.swing.JButton();

        // Link the controller to the buttons on the form
        this.clearButton.addActionListener(controller);
        this.saveButton.addActionListener(controller);
        this.closeButton.addActionListener(controller);

        /**
         * set close function to dispose of the form
         */
        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        /**
         * set the form layout to null, we'll place the components manually
         * using x,y layout positions
         */
        getContentPane().setLayout(null);

        /**
         * set the Room Number field label
         */
        roomNumberLabel.setText("Room Number:");
        /**
         * add Room Number field label to form
         */
        getContentPane().add(roomNumberLabel);
        roomNumberLabel.setBounds(30, 20, 120, 14);

        /**
         * add Room Number input field to form
         */
        getContentPane().add(roomNumberTextfield);
        roomNumberTextfield.setBounds(120, 20, 190, 20);
        
        /**
         * set Room Type field label
         */
        roomTypeLabel.setText("Room Type:");
        
        /**
         * add Room Type field to form
         */
        getContentPane().add(roomTypeLabel);
        roomTypeLabel.setBounds(30, 50, 100, 14);
         
        /**
         * Create the array of values for the data model that is linked to the
         * drop down list of room types
         */
        String[] datamodelValues = {"LAB", "CLASSROOM", "LECTURE HALL"};
        /**
         * Create the data model and store the array in it
         */
        DefaultComboBoxModel comboboxDataModel = new DefaultComboBoxModel(datamodelValues);
        
        /**
         * Link the data model to the combo box
         */
        roomTypeCombobox.setModel(comboboxDataModel);
        
        /**
         * Add the combo box to the form
         */
        getContentPane().add(roomTypeCombobox);
        roomTypeCombobox.setBounds(120, 50, 190, 20);

        /**
         * set the Room Capacity field label
         */
        roomCapacityLabel.setText("Room Capacity:");
        
        /**
         * add  Room Capacity field label to form
         */
        getContentPane().add(roomCapacityLabel);
        roomCapacityLabel.setBounds(30, 80, 120, 14);
        
        /**
         * add Room Capacity field to form
         */
        getContentPane().add(roomCapacityTextField);
        roomCapacityTextField.setBounds(120, 80, 100, 20);

        /**
         * set the button label
         */
        saveButton.setText("Save");
        /**
         * add button to form
         */
        getContentPane().add(saveButton);
        saveButton.setBounds(40, 120, 100, 23);

        /**
         * set the button label
         */
        clearButton.setText("Clear");
        /**
         * add button to form
         */
        getContentPane().add(clearButton);
        clearButton.setBounds(150, 120, 90, 23);

        /**
         * set the button label
         */
        closeButton.setText("Close");
        /**
         * add button to form
         */
        getContentPane().add(closeButton);
        closeButton.setBounds(250, 120, 90, 23);

        // Set Size after creating
        this.setSize(400, 200);
    }

    // accessor methods for controller class to use to get the values from the form
    public JTextField getRoomNumberTextfield() {
        return roomNumberTextfield;
    }

    public JComboBox getRoomTypeCombobox() {
        return roomTypeCombobox;
    }
    
     public JTextField getRoomCapacityTextField() {
        return roomCapacityTextField;
    }

}
