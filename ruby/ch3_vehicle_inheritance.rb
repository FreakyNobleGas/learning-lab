# Description: Using inheritance, create a base vehicle class and extend it with other classes
#              based on the unique features of a specific vehicle

# Parent SuperClass
class Vehicle
  attr_accessor :odometer, :gas_used

  def accelerate
    puts "Floor it!"
  end

  def sound_horn
    puts "Beep! Beep!"
  end

  def steer
    puts "Turn front 2 wheels."
  end

  def mileage
    return @odometer / @gas_used
  end

  def to_s
    "A Vehicle with #{@odometer} miles on it"
  end
end

# < reads as "Car Inherits from Vehicle" or "Car Specializes Vehicle"
class Car < Vehicle

end

class Truck < Vehicle
  attr_accessor :cargo

  def load_bed(contents)
    puts "Loading #{contents} into the truck bed."
    @cargo = contents
  end

end

class Motorcycle < Vehicle
  # Note: steer is overriding parent class steer method
  def steer
    puts "Turn front wheel."
  end

  def accelerate
    # super will call the superclass method. This is helpful if we want to extend the logic.
    super
    puts "Does a wheelie!"
  end
end

puts "### Creating Truck ###"
truck = Truck.new
truck.steer
truck.accelerate
truck.load_bed("bananas")

puts "\n### Creating Car ###"
car = Car.new
car.odometer = 10000
car.gas_used = 200
puts "Lifetime Mileage #{car.mileage}"

puts "\n### Creating Motorcycle ###"
motorcycle = Motorcycle.new
motorcycle.steer
motorcycle.accelerate
puts "Motorcycle's parent class is #{Motorcycle.superclass}."

puts "\n### Creating Vehicle ###"
vehicle = Vehicle.new
vehicle.odometer = 120000
# Calls .to_s method
puts vehicle