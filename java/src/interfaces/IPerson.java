package interfaces;

import java.time.LocalDate;
import exceptionhandlers.InvalidDataException;

public interface IPerson {

	public String getName();
	public void setName(String p_name) throws InvalidDataException;
	public String getAddress();
	public void setAddress(String p_address) throws InvalidDataException;
	public LocalDate getDateOfBirth();
	public void setDateOfBirth(LocalDate dateOfBirth) throws InvalidDataException;
}
