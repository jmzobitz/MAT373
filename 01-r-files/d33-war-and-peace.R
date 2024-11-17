# MAT 373, Day 33
# Read in a large book (War and Peace), break into two characters, and then
# Determine the probability of the two characters occurring.

# Source: https://maximilianrohde.com/posts/code-breaking-with-metropolis/#defining-an-english-similarity-score

# Read in war and peace from project gutenberg:
war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")

# Replace all upper case to lower, make any adjustments to the letters
war_and_peace <-
  war_and_peace |>
  stringr::str_to_lower() |>
  gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  stringi::stri_trans_general(id = "Latin-ASCII")

# Define a function that breaks the text consecutively into two letters
break_into_two_chars <- function(text){
  starting_indices <- 1 : (nchar(text) - 1)
  ending_indices <- starting_indices + 1
  return(stringi::stri_sub(text,
                           from = starting_indices,
                           to = ending_indices))
}

war_and_peace_2_characters <- break_into_two_chars(war_and_peace)



# Ten most common two-character combinations
probability_table <-
  table(war_and_peace_2_characters) / length(war_and_peace_2_characters)

# Save the probability table and characters for evaluation
save(war_and_peace_2_characters,probability_table,file='01-r-files/metropolis-hastings-cipher.Rda')



