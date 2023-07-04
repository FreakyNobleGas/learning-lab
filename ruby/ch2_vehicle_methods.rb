# Description: Design methods for a virtual test-drive app which lets customers
#              test drive their car without going to the showroom.
#
# Note: You can call these methods in irb by navigating to the directory with this
#       file, typing `irb -I .` and typing `require "vehicle_methods"`

# Define custom methods for testing vehicle features
def accelerate
  puts "vroom vroom"
end

def sound_horn
  puts "beep beep"
end

def use_headlights(brightness="low beams")
  puts "Turning on #{brightness} headlights"
end

def calculate_mileage(miles_driven, gas_used)
  if gas_used == 0
    return 0
  end
  return miles_driven / gas_used
end

# Call methods to showcase vehicle features
accelerate
sound_horn

# Call use_headlights with default parameter, then specify our own
use_headlights
use_headlights("high beam")

lifetime_mileage = calculate_mileage(120000, 900)
trip_mileage = calculate_mileage(400, 15)
puts "Lifetime Mileage is #{lifetime_mileage}. Trip Mileage is #{trip_mileage}."

# Test brand new car
new_car_mileage = calculate_mileage(0, 0)
puts "New car mileage is #{new_car_mileage}"