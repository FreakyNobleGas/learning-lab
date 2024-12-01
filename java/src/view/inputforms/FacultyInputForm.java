package view.inputforms;

import controllers.InputFacultyFormController;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class FacultyInputForm extends javax.swing.JFrame  {

// Form fields

    private javax.swing.JTextField addressField;
    private javax.swing.JButton clearButton;
    private javax.swing.JButton closeButton;
    private javax.swing.JSpinner dateOfBirthDay;
    private javax.swing.JSpinner dateOfBirthMonth;
    private javax.swing.JSpinner dateOfBirthYear;
    private javax.swing.JSpinner dateOfHireDay;
    private javax.swing.JSpinner dateOfHireMonth;
    private javax.swing.JSpinner dateOfHireYear;
    private javax.swing.JFormattedTextField jFormattedTextField1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField nameField;
    private javax.swing.JTextField salaryField;
    private javax.swing.JButton saveButton;
    private javax.swing.JComboBox statusField;

    /**
     * default constructor
     */
    public FacultyInputForm(InputFacultyFormController controller) {

        // Initialize all the components on the form
        initComponents();
        
         // Link the controller to the buttons on the form
        this.clearButton.addActionListener(controller);
        this.saveButton.addActionListener(controller);
        this.closeButton.addActionListener(controller);
                 
        // Set the size of the window
        this.setSize(600, 600);
        
        // Make the form visible
        this.setVisible(true);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     */
    @SuppressWarnings("unchecked")
    
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jFormattedTextField1 = new javax.swing.JFormattedTextField();
        nameField = new javax.swing.JTextField();
        addressField = new javax.swing.JTextField();
        closeButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        saveButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        dateOfBirthYear = new javax.swing.JSpinner();
        dateOfBirthMonth = new javax.swing.JSpinner();
        dateOfBirthDay = new javax.swing.JSpinner();
        dateOfHireDay = new javax.swing.JSpinner();
        dateOfHireMonth = new javax.swing.JSpinner();
        dateOfHireYear = new javax.swing.JSpinner();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        statusField = new javax.swing.JComboBox();
        salaryField = new javax.swing.JTextField();
        jLabel11 = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();

        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jScrollPane1.setViewportView(jTextArea1);

        jFormattedTextField1.setText("jFormattedTextField1");

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().setLayout(null);
        getContentPane().add(nameField);
        nameField.setBounds(160, 50, 200, 30);
        getContentPane().add(addressField);
        addressField.setBounds(160, 90, 200, 30);

        closeButton.setText("Close");
        
        getContentPane().add(closeButton);
        closeButton.setBounds(400, 110, 110, 23);

        jLabel1.setText("Add Faculty");
        getContentPane().add(jLabel1);
        jLabel1.setBounds(220, 10, 150, 14);

        jLabel2.setText("Name:");
        getContentPane().add(jLabel2);
        jLabel2.setBounds(10, 50, 130, 14);

        jLabel3.setText("Address:");
        getContentPane().add(jLabel3);
        jLabel3.setBounds(10, 90, 140, 14);

        jLabel5.setText("Data Of Birth");
        getContentPane().add(jLabel5);
        jLabel5.setBounds(10, 190, 140, 20);

        jLabel6.setText("Data Of Hire");
        getContentPane().add(jLabel6);
        jLabel6.setBounds(10, 240, 120, 20);

        saveButton.setText("Save");
        getContentPane().add(saveButton);
        saveButton.setBounds(400, 50, 110, 23);

        clearButton.setText("Clear");
        getContentPane().add(clearButton);
        clearButton.setBounds(400, 80, 110, 23);

        dateOfBirthYear.setModel(new javax.swing.SpinnerListModel(new String[] {"1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"}));
        getContentPane().add(dateOfBirthYear);
        dateOfBirthYear.setBounds(160, 190, 60, 30);

        dateOfBirthMonth.setModel(new javax.swing.SpinnerListModel(new String[] {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}));
        getContentPane().add(dateOfBirthMonth);
        dateOfBirthMonth.setBounds(240, 190, 50, 30);

        dateOfBirthDay.setModel(new javax.swing.SpinnerListModel(new String[] {"01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"}));
        getContentPane().add(dateOfBirthDay);
        dateOfBirthDay.setBounds(310, 190, 50, 30);

        dateOfHireDay.setModel(new javax.swing.SpinnerListModel(new String[] {"01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"}));
        getContentPane().add(dateOfHireDay);
        dateOfHireDay.setBounds(310, 240, 50, 30);

        dateOfHireMonth.setModel(new javax.swing.SpinnerListModel(new String[] {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}));
        getContentPane().add(dateOfHireMonth);
        dateOfHireMonth.setBounds(240, 240, 50, 30);

        dateOfHireYear.setModel(new javax.swing.SpinnerListModel(new String[] {"1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"}));
        getContentPane().add(dateOfHireYear);
        dateOfHireYear.setBounds(160, 240, 60, 30);

        jLabel8.setText("Month");
        getContentPane().add(jLabel8);
        jLabel8.setBounds(244, 170, 40, 14);

        jLabel9.setText("Year");
        getContentPane().add(jLabel9);
        jLabel9.setBounds(180, 170, 30, 14);

        jLabel10.setText("Day");
        getContentPane().add(jLabel10);
        jLabel10.setBounds(320, 170, 30, 14);

        jLabel12.setText("Status");
        getContentPane().add(jLabel12);
        jLabel12.setBounds(10, 300, 130, 14);

        statusField.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Full Time", "Part Time" }));
        getContentPane().add(statusField);
        statusField.setBounds(160, 300, 130, 20);
        getContentPane().add(salaryField);
        salaryField.setBounds(160, 350, 130, 20);

        jLabel11.setText("Salary");
        getContentPane().add(jLabel11);
        jLabel11.setBounds(10, 350, 130, 14);

        jLabel13.setText("Enter Single Digits, no spaces, no commas");
        getContentPane().add(jLabel13);
        jLabel13.setBounds(160, 380, 330, 14);

        pack();
   }   
    

    public JTextField getAddressField() {
        return addressField;
    }

    public JButton getClearButton() {
        return clearButton;
    }

    public JSpinner getDateOfBirthDay() {
        return dateOfBirthDay;
    }

    public JSpinner getDateOfBirthMonth() {
        return dateOfBirthMonth;
    }

    public JSpinner getDateOfBirthYear() {
        return dateOfBirthYear;
    }

    public JSpinner getDateOfHireDay() {
        return dateOfHireDay;
    }

    public JSpinner getDateOfHireMonth() {
        return dateOfHireMonth;
    }

    public JSpinner getDateOfHireYear() {
        return dateOfHireYear;
    }
    
    public JButton getExitButton() {
        return closeButton;
    }

    public JFormattedTextField getjFormattedTextField1() {
        return jFormattedTextField1;
    }

    public JScrollPane getjScrollPane1() {
        return jScrollPane1;
    }

    public JTextArea getjTextArea1() {
        return jTextArea1;
    }

    public JTextField getNameField() {
        return nameField;
    }

    public JTextField getSalaryField() {
        return salaryField;
    }

    public JButton getSaveButton() {
        return saveButton;
    }
   
    public JComboBox getStatusField() {
        return statusField;
    }
    
}
