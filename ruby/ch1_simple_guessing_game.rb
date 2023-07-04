# Description: Simple guessing game where the user is prompted to guess a number between 0 - 100. The program will
#              let the user know if the guess is correct, too HIGH, or too LOW. If incorrect, the user is allowed
#              to try again up to 10 times.

# `puts` is short for "put string" and `gets` is short for "get string"
puts "Welcome to the Simple Guessing Game!\nWhat is your name?"

# chomp method removes newline char for string objects
name = gets.chomp
puts "Welcome, #{name}!"

# rand returns up to max - 1. Add 1 so that 100 is included
num_to_guess = rand(100) + 1
num_of_guesses = 0
total_allowed_guesses = 2
# Keep track if user guess correctly
guessed_it = false

# Loop using `until` keyword for better readability
until num_of_guesses == total_allowed_guesses || guessed_it
  puts "\nYou have #{total_allowed_guesses - num_of_guesses} guesses remaining."
  puts "I've got a number from 0 to 100, can you guess what it is?"
  guess = gets.to_i

  if guess == num_to_guess
    puts "You won!"
    guessed_it = true
  elsif guess < num_to_guess
    puts "Your guess is TOO low."
  elsif guess > num_to_guess
    puts "Your guess is to TOO high."
  end
  num_of_guesses += 1
end

unless guessed_it
  puts "Better luck next time! The correct number was #{num_to_guess}."
else
  puts "Good job, #{name}!"
end