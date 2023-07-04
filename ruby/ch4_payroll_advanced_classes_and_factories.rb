# Description: Create a payroll system for employees that uses validation and initialization methods.

class Employee
  attr_reader :name

  def initialize(name = "Anonymous")
    self.name = name
  end

  def name=(name)
    if name == ""
      raise "Name can't be blank!"
    end
    @name = name
  end

  def print_name
    puts "Name: #{@name}"
  end
end

class SalariedEmployee < Employee
  attr_reader :salary

  def initialize(name, salary = 0.0)
    super(name)
    self.salary = salary
  end

  def salary=(salary)
    if salary < 0
      raise "A salary of #{salary} isn't valid!"
    end
    @salary = salary
  end

  def print_pay_stub
    self.print_name

    # Two week pay period
    pay_for_period = (@salary / 365.0) * 14
    # Format sequence types are: %s string, %i integer, %f floating-point decimal
    puts format("Pay this period: %0.2f", pay_for_period)
  end

end

class HourlyEmployee < Employee

  attr_reader :hours_per_week, :hourly_rate

  # Factories for common occupations
  def self.security_guard(name)
    HourlyEmployee.new(name, 19.25, 30)
  end

  def self.cashier(name)
    HourlyEmployee.new(name, 12.75, 25)
  end

  def self.janitor(name)
    HourlyEmployee.new(name, 10.50, 20)
  end

  def initialize(name, hours_per_week = 0, hourly_rate = 0)
    super(name)
    self.hours_per_week = hours_per_week
    self.hourly_rate = hourly_rate
  end

  def hours_per_week=(hpw)
    if hpw < 0
      raise "Hours per a week can't be negative!"
    end
    @hours_per_week = hpw
  end

  def hourly_rate=(hr)
    if hr < 0
      raise "Hourly rate can't be negative!"
    end
    @hourly_rate = hr
  end

  def print_pay_stub
    print_name
    pay_for_period = hourly_rate * hours_per_week * 2
    puts "Pay this period: #{format("%.2f", pay_for_period)}"
  end
end

se = SalariedEmployee.new("Jane Doe", 100000)
se.print_pay_stub

he = HourlyEmployee.new("John Doe", 40, 1)
he.print_pay_stub

# Create employees using factories defined as class methods
sg = HourlyEmployee.security_guard("Angela Matthews")
sg.print_pay_stub

ja = HourlyEmployee.janitor("Edwin Burgess")
ja.print_pay_stub

ca = HourlyEmployee.cashier("Ivan Stokes")
ca.print_pay_stub