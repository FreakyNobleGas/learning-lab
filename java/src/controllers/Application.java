/*
 * This is the main entry into the application. It creates a menu controller object
 * and the controller object creates the forms and the data models as needed.
 * It also sets up debug logging 
 */
package controllers;

import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class Application {

	// Create the logger for debugging
	// DEBUG_LOGGER will create log records
	// DEBUG_LOGGER is taking the name of the class to use in the log messages
	// This helps determine where the message came from (this is optional)
	private static final Logger DEBUG_LOGGER = Logger.getLogger(Application.class.getName());

	// Declare the file handler for debugging. It will be created in a try/catch
	// block below
	// DEBUG_FILEHANDLER will log the messages created by DEBUG_LOGGER, to a file
	private static FileHandler DEBUG_FILEHANDLER;

	/**
	 * main entry into the application Note that there are 2 input arguments:
	 * args[0] - location of persisted data args[1] - name and location of debug
	 * file
	 */
	public static void main(String[] args) {

		try {

			// Create a debug log file with the name and location specified by args[1]
			// (This should look somewhat familiar)
			DEBUG_FILEHANDLER = new FileHandler(args[1]);

			// Unlike the console output, file output needs to be formatted!
			// We will use a simple formatter class for the log file
			DEBUG_FILEHANDLER.setFormatter(new SimpleFormatter());

			// Set the default level of logging in the debug logger (can be changed by user)
			DEBUG_LOGGER.setLevel(Level.SEVERE);

			// Link the debug file handler to the debug logger
			DEBUG_LOGGER.addHandler(DEBUG_FILEHANDLER);

		} catch (IOException e) {
			// OOPS! Something's wrong with one of the log files, halt processing!
			e.printStackTrace();
			System.exit(0);
		}

		// If the log files get created correctly, show the UI
		// Create main menu controller, the controller creates the menu form
		MainMenuController controller = new MainMenuController(args[0], args[1]);

		// Retrieve the main menu form from the controller and make it visible
		controller.getMainMenu().setVisible(true);

	}

	// Get methods for the loggers
	public static Logger getDEBUG_LOGGER() {
		return DEBUG_LOGGER;
	}

}
