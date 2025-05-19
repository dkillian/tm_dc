# Datacamp 
# Text mining track
# String manipulation (2)

# CHAPTER 1 ---- 

# strings ---- 

"hi!"

"I said "hi"" # repeated double quotes don't work 
'I said "hi"' # enclose double quotes with single quotes
"I'd say \"hi\"" # double and single quotes, use double quotes with escape

# Define line1
line1 <- "The table was a large one, but the three were all crowded together at one corner of it:"

# Define line2
line2 <- '"No room! No room!" they cried out when they saw Alice coming.'

# Define line3
line3 <- "\"There's plenty of room!\" said Alice indignantly, and she sat down in a large arm-chair at one end of the table."

# Should display: To have a \ you need \\
writeLines("To have a \\ you need \\\\")

# Should display: 
# This is a really 
# really really 
# long string
writeLines("This is a really\nreally really\nlong string")

# Use writeLines() with 
# "\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e"
writeLines("\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e")

# numbers ----

estimate <- 1.340190290100
estimate

as.character(estimate)
format(estimate, digits=8)

formatC(estimate, 
        format="f",
        digits=2)

198900000000000000000000000000000000000000000

0.000000000000000000000000000000000008

x <- c(198900000000000000000000000000000000000000000, 0.000000000000000000000000000000000008)
x

format(x, scientific=F)

formatC(x, format="f")

formatC(x, format="e")

formatC(x, format="g")

1e3
1e-3
1e+3


# Some vectors of numbers
percent_change  <- c(4, -1.91, 3.00, -5.002)
income <-  c(72.19, 1030.18, 10291.93, 1189192.18)
p_values <- c(0.12, 0.98, 0.0000191, 0.00000000002)

# Format c(0.0011, 0.011, 1) with digits = 1
format(c(0.0011, 0.011, 1), digits=1)

# Format c(1.0011, 2.011, 1) with digits = 1
format(c(1.0011, 2.011, 1), digits=1)

# Format percent_change to one place after the decimal point
format(percent_change, digits=2)

# Format income to whole numbers
format(income, scientific=F, digits=1)

# Format p_values in fixed format
format(p_values, scientific=F, format="f")


formatted_income <- format(income, digits = 2)

# Print formatted_income
print(formatted_income)

# Call writeLines() on the formatted income
writeLines(formatted_income)

# Define trimmed_income
trimmed_income <- format(income, digits=2, trim=T)

# Call writeLines() on the trimmed_income
writeLines(trimmed_income)

# Define pretty_income
pretty_income <- format(income, digits=2, big.mark=",")

# Call writeLines() on the pretty_income
writeLines(pretty_income)

# From the format() exercise
x <- c(0.0011, 0.011, 1)
y <- c(1.0011, 2.011, 1)

# formatC() on x with format = "f", digits = 1
formatC(x, format="f", digits=1)

# formatC() on y with format = "f", digits = 1
formatC(y, format="f", digits=1)

# Format percent_change to one place after the decimal point
formatC(percent_change, format="f", digits=1)

# percent_change with flag = "+"
formatC(percent_change, format="f", digits=1, flag="+")

# Format p_values using format = "g" and digits = 2
formatC(p_values, format="g", digits=2)


# Add $ to pretty_income
paste("$", pretty_income, sep="")

# Add % to pretty_percent
paste(pretty_percent, "%", sep="")

# Create vector with elements like 2010: +4.0%`
year_percent <- paste(years, ": ", pretty_percent, "%", sep="")

# Collapse all years into single string
paste(year_percent, collapse=", ")

# Define the names vector
income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")

# Create pretty_income
pretty_income <- format(income, digits=2, big.mark=",")

# Create dollar_income
dollar_income <- paste("$", pretty_income, sep="")

# Create formatted_names
formatted_names <- format(income_names, justify="right")

# Create rows
rows <- paste(formatted_names, dollar_income, sep="   ")

# Write rows
writeLines(rows)



# putting strings together ---- 

paste("E", "I", "E", "I", "O")

paste("E", "I", "E", "I", "O", sep="-")

paste(c("Here", "There", "Everywhere"), "a")

animal_goes <- "moo"

paste(c("Here","There","Everywhere"), "a", animal_goes)

paste(c("Here","There","Everywhere"), 
      "a",
      c(animal_goes, animal_goes, # vectorized: moo, moo, moo-moo
        paste(rep(animal_goes,2), collapse="-")),
      collapse=", ")

old_mac <- function(animal, animal_goes){
    eieio <- paste("E", "I", "E", "I", "O", sep = "-")
    old_mac <- "Old MacDonald had a farm"
    writeLines(c(
        old_mac,
        eieio,
        paste("And on his farm he had a", animal),
        eieio,
        paste(c("Here", "There", "Everywhere"), "a",
              c(animal_goes, animal_goes,
                paste(rep(animal_goes, 2), collapse = "-")),
              collapse = ", "),
        old_mac,
        eieio))
}

old_mac("cat", "meow")



# Randomly sample 3 toppings
my_toppings <- sample(toppings, size = 3)
my_toppings

# Print my_toppings
print(my_toppings)
my_toppings[1:2]

# Paste "and " to last element: my_toppings_and
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep="")
my_toppings_and

# Collapse with comma space: these_toppings
these_toppings <- paste(my_toppings_and, collapse=", ")
these_toppings

# Add rest of sentence: my_order
my_order <- paste("I want to order a pizza with ", these_toppings, ".", sep="")

# Order pizza with writeLines()
writeLines(my_order)















