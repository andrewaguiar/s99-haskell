require 'faker'

File.open('corpus.txt', 'w') do |f|
  10000.times do |n|
    f << "#{n},#{Faker::Lorem.paragraph((5..12).to_a.sample)}\n"
  end
end
