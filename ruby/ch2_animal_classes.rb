# Description: Create classes that represent specific animals and actions they might take.

# Class names in ruby are camelcase
class Dog

  # Attribute writer method
  def name=(n)
    if n == ""
      raise "Name can't be empty!"
    end
    # Attributes are variables stored within the class. They cannot be accessed outside of the class
    @name = n
  end

  # Attribute reader method
  def name
    @name
  end

  # Shorthand notation for attribute reader. Keep longhand version for writer so we can add validation
  def age=(a)
    if a < 0
      raise "Age can't be less than zero!"
    end
    @age = a
  end
  attr_reader :age

  def speak
    puts "#{@name} barks! Woof Woof."
  end

  # Shorthand notation for attribute writers and readers
  # attr_accessor automatically creates both a writer and reader method.
  # attr_writer creates attribute writer method
  # attr_reader creates attribute reader method
  # All 3 shorthands can take multiple values.
  # For example, `attr_accessor :age, :breed`
end

# Use .new method to create an instance of the class
sandy = Dog.new
sandy.name = "Sandy"
sandy.speak

# Uncomment to test exception
#sandy.name = ""
#sandy.speak
