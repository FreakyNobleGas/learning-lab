# Description: Calculate store prices using blocks and the array's each method.

def calculate_price(arr)
  amount = 0
  arr.each do |price|
    amount = yield amount, price
  end
  amount
end

items = [20,30,50]
total = calculate_price(items) do |amount, price|
  amount += price
end
puts format("Total is %.2f", total)

discount = calculate_price(items) do |amount, price|
  amount += (price * 0.5)
end
puts "Total after 50% discount is #{format("%.2f", discount)}"

refund = calculate_price(items) do |amount, price|
  amount -= price
end
puts "You're refund is #{format("%.2f", refund)}"