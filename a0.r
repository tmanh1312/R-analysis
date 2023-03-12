# A0 foundational skills 
# Hi, this is MIA TRUONG
# Set up and Defining variables------------------------------------------------
name <- "Mia"

# Load the the `stringr` package 
library ("stringr")

# Create a numeric variable `my_age` that is equal to your age
my_age <- 22

# Create a variable `my_name` that is equal to your first name
my_name <- "Mia"

# Using multiplication, create a variable `minutes_in_a_day` that is
# equal to the number of minutes in a day
minutes_in_a_day <- 60*24

# Using multiplication, create a variable `hours_in_a_year` that is
# equal to the number of hours in a year
hours_in_a_year <- 24*365

# Working with functions -------------------------------------------------------

# Write a function `make_introduction()` that takes in two args (`name`, `age`) 
make_introduction <- function (name, age) {
  cat ("Hello, my name is", name,"and I'm", age, "years old.") 
}
make_introduction ("Mia", "22")


# This function should return a string value that says:
# Hello, my name is {name}, and I'm {age} years old.
# The values {name} and {age} should take on the values passed into the function 
# Make sure that proper spacing is used (e.g., you shouldn't have multiple
# spaces between words, and you should a space after a comma)

cat ("\n")
# Create a variable `my_intro` by passing your variables `my_name` and `my_age`
# into your `make_introduction()` function
my_intro <- make_introduction(my_name, my_age)
print(my_intro)

# Using the `str_count` function, create a variable `occurrences` that stores
# the # of times the letter "e" appears in `my_intro`
occurrences <- str_count(my_intro,"e")
print(occurrences)

# Create a function `inches_to_cm` that converts from inches to centimeters and 
# returns the converted value in cm
inches_to_cm <- function (inches) {
  in_to_cm = inches*2.54
  cat (inches,"inch(es) equals to",in_to_cm, "cm")
}
inches_to_cm (2)

# Create a variable `inches_tall` that is your (numeric) height in inches
inches_tall <- 62.2

cat ("\n")
# Using your `inches_to_cm` function and your `inches_tall` variable, 
# create a variable `cm_tall` that is your height in centimeters
cm_tall <- inches_to_cm(inches_tall)
print (cm_tall)

# Write a function `has_more_zs` to determine which of two strings contains 
# more instances of the letter "z". It should take as parameters two string
# variables, and return the argument which has more occurances of the letter "z"
# If neither phrase contains the letter "z", it should return:
# "Neither string contains the letter z."
# If the phrases contain the same number of "z"s, it should return:
# "The strings have the same number of Zs." 
# The function must work for both capital and lowercase "z"s. 
has_more_zs <- function (p1, p2) {
  p1_zs_count <- str_count(p1,"z")
  p2_zs_count <- str_count(p2,"z")
  if(p1_zs_count==0 && p2_zs_count==0) {
    print ("Neither string contains letter z")
  } else if(p1_zs_count==p2_zs_count) {
      print("The strings have the same number of Zs")
  } else if(p1_zs_count > p2_zs_count) {
      print(p1)
  } else
      print(p2)   
}

# Create a variable `more_zs` by passing two strings of your choice to your
# `has_more_zs` function
more_zs <- has_more_zs ("Hello zz","Hello there's no zzzz")

# Write a function `remove_digits` that will remove all digits
# (i.e., 0 through 9) from all elements in a *vector of strings*.
remove_digits <- function(vectorstrings){
  x <- "0123456789"
  gsub("[0-9]", replacement = " ", x = vectorstrings)
}

# Demonstrate that your approach is successful by passing a vector of courses
# to your function. For example, remove_digits(c("INFO 201", "CSE 142"))
print(remove_digits(c("INFO 201", "CSE 142")))

# Write a function `fluidConversion()` that takes in one numeric arg (`fluid_oz`) 
# that converts any number of fluid ounces to the equivalent 
# number of gallons, quarts, pints, and gills. Your function should print the 
# calculated number of gallons, quarts, pints, and gills on one line; be sure to
# add appropriate labels to the display values so the user knows what the value 
# means (e.g. if the user enters 6523 fluid ounces, you should display “6523 
# fluid ounces is 50 gallon(s), 3 quart(s), 1 pint(s), and 2 gill(s)). Use the 
# following hints for your conversion: 
# There are 128 fluid ounces in a gallon.
# There are 32 fluid ounces in a quart.
# There are 16 fluid ounces in a pint.
# There are 4 fluid ounces in a gill.
# The modulo (a.k.a. remainder) operator in R is %% and will show the remainder 
# left after an integer division. 
fluidConversion <- function(fluid_oz) {
  gallon <- floor(fluid_oz / 128)
  quart <- floor((fluid_oz %% 128) / 32)
  pint <- floor(((fluid_oz %% 128) %% 32) / 16)
  gill <- floor((((fluid_oz %% 128) %% 32) %% 16) / 4)
  cat (fluid_oz, "fluid ounces is", gallon, "gallon(s),", quart, "quart(s)", pint, "pint(s), and", gill, "gill(s)." )
}
fluidConversion(1000)

cat("\n")
# Write a function `parks_and_rec()` that takes in two args (`weeks`, `hrs`) to 
# calculate how many episodes of the show "Parks and Rec" you can watch within a
# given time frame. The weeks arg is how many weeks they have to watch and the
# hrs arg is how many hours of TV they watch a day. Assuming that each episode 
# is 21 minutes long, how many episodes can the user watch? 
# Return the number of episodes as an numeric type.
parks_and_rec <- function(weeks, hrs) {
  episodes <- (weeks * 7 * hrs * 60) / 21
  cat ("The total number of episodes the user can watch is", episodes)
}
parks_and_rec(2,2.5)

# Vectors ----------------------------------------------------------------------

# Create a vector `movies` that contains the names of six movies you like
movies <- c("Titanic", "2521", "Bridgerton", "Home", "Up", "Shrek")

# Create a vector `top_three` that only contains the first three movies
# You should do this by subsetting the vector, not by simply retyping the movies
top_three <- movies[c(1:3)]


# Using your vector and the `paste()` method, create a vector `excited` that
# adds the phrase - " is a great movie!" to the end of each element `movies`
excited <- paste(movies, " is a great movie!", sep="")

# Create a vector `without_four` by omitting the fourth element from `movies`
# You should do this using a _negative index_ 
without_four <- movies[-4]
