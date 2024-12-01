package view;

import controllers.MainMenuController;

public class MainMenu extends javax.swing.JFrame {

    // Menu form components
    // Top level menu options
    private javax.swing.JMenu classroomMenuOption = new javax.swing.JMenu("Classrooms");
    private javax.swing.JMenu fileMenuOption = new javax.swing.JMenu("File");
    private javax.swing.JMenu coursesMenuOption = new javax.swing.JMenu("Courses");
    private javax.swing.JMenu facultyMenuOption = new javax.swing.JMenu("Faculty");
    private javax.swing.JMenu studentMenuOption = new javax.swing.JMenu("Student");

    // Sub-menu choices
    private javax.swing.JMenuItem addClassroom = new javax.swing.JMenuItem("Add Classroom");
    private javax.swing.JMenuItem listClassrooms = new javax.swing.JMenuItem("List Classrooms");
    
    private javax.swing.JMenuItem exitApplication = new javax.swing.JMenuItem("Exit");
    private javax.swing.JMenuItem saveData = new javax.swing.JMenuItem("Save Data");
    private javax.swing.JMenuItem loadData = new javax.swing.JMenuItem("Load Data");
    private javax.swing.JMenu setLogLevel = new javax.swing.JMenu("Set Log Level");
    
    // Notice that the logging level menu choice is another menu with menu itmes
    private javax.swing.JMenuItem logFinest = new javax.swing.JMenuItem("Log All");
    private javax.swing.JMenuItem logInfo = new javax.swing.JMenuItem("Log Info");
    private javax.swing.JMenuItem logWarning = new javax.swing.JMenuItem("Log Warning");
    private javax.swing.JMenuItem logSevere = new javax.swing.JMenuItem("Log Severe");

    private javax.swing.JMenuItem addOfferredCourse = new javax.swing.JMenuItem("Add Course");
    private javax.swing.JMenuItem listOfferredcourse = new javax.swing.JMenuItem("List Courses");
    
    private javax.swing.JMenuItem addFaculty = new javax.swing.JMenuItem("Add Faculty");
    private javax.swing.JMenuItem listFaculty = new javax.swing.JMenuItem("List Faculty");
    
    private javax.swing.JMenuItem addStudent = new javax.swing.JMenuItem("Add Student");
    private javax.swing.JMenuItem listStudent = new javax.swing.JMenuItem("List Student");
    
    // Menu bar that contains the top level menu options
    private javax.swing.JMenuBar menuBar = new javax.swing.JMenuBar();

    /**
     * Constructor
     */
    public MainMenu(MainMenuController controller) {

        // Add the top level menu choices to the menu bar
        menuBar.add(fileMenuOption);
        menuBar.add(classroomMenuOption);
        menuBar.add(coursesMenuOption);
        menuBar.add(facultyMenuOption);
        menuBar.add(studentMenuOption);
        
        // Add menu items to log level menu
        setLogLevel.add(logFinest);
        setLogLevel.add(logInfo);
        setLogLevel.add(logWarning);
        setLogLevel.add(logSevere);
        
        // Add the sub menu items
        fileMenuOption.add(saveData);
        fileMenuOption.add(loadData);
        fileMenuOption.add(setLogLevel);
        fileMenuOption.add(exitApplication);
        classroomMenuOption.add(addClassroom);
        classroomMenuOption.add(listClassrooms);
        
        coursesMenuOption.add(addOfferredCourse);
        coursesMenuOption.add(listOfferredcourse);
        
        facultyMenuOption.add(addFaculty);
        facultyMenuOption.add(listFaculty);
        
        studentMenuOption.add(addStudent);
        studentMenuOption.add(listStudent);
        
        // Link the menu bar to the form
        setJMenuBar(menuBar);

        // Once the form is created, link this controller to the sub menu items
        addClassroom.addActionListener(controller);
        listClassrooms.addActionListener(controller);
        
        exitApplication.addActionListener(controller);
        saveData.addActionListener(controller);
        loadData.addActionListener(controller);
        logFinest.addActionListener(controller);
        logInfo.addActionListener(controller);
        logWarning.addActionListener(controller);
        logSevere.addActionListener(controller);
        
        addOfferredCourse.addActionListener(controller);
        listOfferredcourse.addActionListener(controller);
        
        addFaculty.addActionListener(controller);
        listFaculty.addActionListener(controller);
        
        addStudent.addActionListener(controller);
        listStudent.addActionListener(controller);
        
        /**
         * set close function to dispose of the form
         */
        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        // Set Size of form after creating
        setSize(400, 150);

    }

}
