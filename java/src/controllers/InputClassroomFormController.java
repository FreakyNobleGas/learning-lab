/*
 * Listens for events on the input form. 
 * Implements the ActionListener interface which contains a single method, 
 * "actionPerformed" - this method contains all the logic to process the data
 * on the form, as well as several other events
 */
package controllers;

import datamodels.Classroom;
import java.awt.event.ActionListener;
import javax.swing.ComboBoxModel;
import view.inputforms.ClassroomInputForm;
import datacontainers.ClassroomDC;
import exceptionhandlers.ErrorPopup;
import exceptionhandlers.InvalidDataException;

public class InputClassroomFormController implements ActionListener {

    // The Classroom data model is passed in via the constructor
    private ClassroomDC m_classroomDataContainer;

    // The form is created here and this constructor object gets passed to it
    private ClassroomInputForm form;

    // Constructor 
    public InputClassroomFormController(ClassroomDC p_classroomDataContainer) {

        // Store the passed in data model
        this.m_classroomDataContainer = p_classroomDataContainer;

        // create the form
        form = new ClassroomInputForm(this);

        // make the form visible
        this.form.setVisible(true);
    }

    /**
     * Implements actionPerformed method of the ActionListener interface
     */
    public void actionPerformed(java.awt.event.ActionEvent event) {

        if (event.getActionCommand().equals("Save")) {
            this.saveData();
        } else if (event.getActionCommand().equals("Clear")) {
            this.clearForm();
        } else if (event.getActionCommand().equals("Close")) {
            this.closeForm();
        }
    }

    /**
     * Private save method If an error is thrown, handle it by creating an error
     * popup and don't save the classroom
     */
    private void saveData() {

        // Create a new classroom object
        Classroom aClassroom = new Classroom();

        // TO-DO - implement the try/catch block for data validation
        try {
            // Retrieve the room numer from the form
            String roomNumber = form.getRoomNumberTextfield().getText();

            // If the room number is valid, store it in the classroom object
            // If not, show the error popup window
            aClassroom.setRoomNumber(roomNumber);

            // Retrieve the data model associated with the drop down list of classrooms
            ComboBoxModel datamodel = this.form.getRoomTypeCombobox().getModel();
            // Retrieve the selected item from the data model, notice
            // it is stored as type Object, we need to convert it in the next step
            Object selectedItem = datamodel.getSelectedItem();
            // Convert (Cast) the selected item to a String
            String roomType = (String) selectedItem;
            // If the room type is valid, store it in the classroom object
            // If not, show the error popup window       
            aClassroom.setTypeOfRoom(roomType);

            // Test for valid integer 
            int capacity = 0;
            try {
                capacity = Integer.parseInt(form.getRoomCapacityTextField().getText());
            } catch (Exception exp) {
                throw new InvalidDataException("Room capacity is not a valid value");
            }
            // if valid but less than zero, show error and set to zero,
            // otherwise, set value
            aClassroom.setCapacity(capacity);
            // Add the new classroom to the list in ClassroomDataModel
            m_classroomDataContainer.getListOfClassrooms().add(aClassroom);          
             
        } catch (InvalidDataException exp) {
            new ErrorPopup(form, exp);

        }

            // Catch our own exception
    }

    /**
     * Private method to clearForm the data
     */
    private void clearForm() {
        form.getRoomNumberTextfield().setText("");
        form.getRoomTypeCombobox().setSelectedIndex(0);
        form.getRoomCapacityTextField().setText("");
    }

    /**
     * Private method to close the form
     */
    private void closeForm() {
        this.form.dispose();
    }

    // gettes to access the private data in the controller object
    public ClassroomDC getDataModel() {
        return m_classroomDataContainer;
    }

    public ClassroomInputForm getForm() {
        return form;
    }
}
