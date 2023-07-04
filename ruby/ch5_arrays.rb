# Description: Learning arrays in ruby

prices = [2.99, 25.00, 9.99]

puts "First item #{prices[0]}"
# Prints entire array
p prices

# You can insert elements anywhere outside of the array boundaries. Empty spaces are filled with nil
puts "\nInsert element after next empty index"
prices[5] = 5.00
p prices

# Helpful built-in Array methods
puts "prices.first "     , prices.first
puts "prices.last "      , prices.last
puts "prices.length "    , prices.length
puts "prices.include? "  , prices.include?(25.00)
puts "prices.find_index ", prices.find_index(9.99)

puts "Push"
prices.push(0.99)
p prices

puts "Pop"
prices.pop
p prices

puts "Shift"
prices.shift
p prices

puts "<< add"
# << operator adds elements to Array
prices << 5.99
prices << 8.99
p prices

puts "Array to String"
# Convert Array to string
puts ["d", "o", "g"].join
puts ["d", "o", "g"].join("-")

puts "String to Array"
# Convert string into Array
p "d-o-g".chars
p "d-o-g".split("-")
