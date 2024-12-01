package exceptionhandlers;

public class InvalidDataException extends Exception {

    public InvalidDataException() {
        super();
    }
    
    public InvalidDataException(String errorMessage) {
        super(errorMessage);
    }
}
