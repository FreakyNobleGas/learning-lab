package exceptionhandlers;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class ErrorPopup {

    /**
     * Constructor takes the following data:
     * 
     * @param badFormData - the form that has bad data
     * @param exception   - the exception that was thrown
     * 
     * Creates a popup window that displays the error
     */
    public ErrorPopup(JFrame badFormData, Exception exception) {
        JOptionPane.showMessageDialog(badFormData,
                exception.getMessage(),
                "Invalid Data",
                JOptionPane.WARNING_MESSAGE);
    }
}
