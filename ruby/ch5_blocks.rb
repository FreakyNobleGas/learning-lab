# Description: Learning how to use blocks in ruby!

# If an ampersand is prefixed to the last variable in a method argument,
# ruby will expect that parameter to be a block.
def my_method(&my_block)
  puts "We're in the method, about to invoke your block!"
  # Calls block of code!
  my_block.call
  puts "We're back in the method"
end

# Block code is ALWAYS defined directly after the method function call
my_method do
  puts "We're in the block!"
end

my_method do
  puts "This is a different block!"
end

# Passing parameters to block
def calculate_sum(&calculator)
  puts "The sum of 10 and 20 is #{calculator.call(10, 20)}"
end
# Block parameters are set between vertical lines instead of parenthesis
calculate_sum do |int1, int2|
  int1 + int2
end

# Using Yield to call block method
def calculate_product
  puts "The product of 4 and 3 is #{yield 4, 3}"
end

calculate_product do
  3 * 4
end

# Favorite colors
def favorite_color
  puts "My favorite color is #{yield "blue"}"
end

favorite_color { |color| color }