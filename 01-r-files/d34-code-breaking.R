### MAT 373, Day 34
### Part 2: Apply MCMC to decode a text

# Define functions:
# (1) Add in the dictionary (for comparison)
# (2) evaluate probability of two characters
# (3) evaluate likelihood
# (4) decode the text (so we can evaluate feasibility)
# (5) randomly swap two characters
# (6) metropolis hastings on the cipher

library(tidyverse)
# Step 1: Define the input data for decoding - this contains the frequence of one or two letter word combinations in war and peace
url <- "https://raw.githubusercontent.com/jmzobitz/MAT373/main/01-r-files/metropolis-hastings-cipher.Rda"

# Step 2: Download the file
download.file(url, destfile = "metropolis-hastings-cipher.Rda", mode = "wb") # Save it locally

# Step 3: Load the file
load("metropolis-hastings-cipher.Rda") # Loads the objects in the .Rda file into your workspace


# We need to define a function that returns the empirical probability of any two-character combination.
get_prob_two_char <- function(two_char){
  prob_from_table <- probability_table[two_char]
  
  # Some two-character combinations that do not occur in War and Peace. We instead approximate the probability by assuming each of them occurred once in the book (1 / length(war_and_peace_2_characters) )
  
  if (is.na(prob_from_table)) {
    return(1 / length(war_and_peace_2_characters))
  } else{
    return(prob_from_table)
  }
}


# Compute the log likelihood of a text string
get_log_lik_text <- function(text){
  
  # Break text into two characters
  starting_indices <- 1 : (nchar(text) - 1)
  ending_indices <- starting_indices + 1
  
  new_text <- stringi::stri_sub(text,
                                from = starting_indices,
                                to = ending_indices)
  
  # Compute the probability
  new_text |>
    purrr::map_dbl(get_prob_two_char) |>
    log() |>
    sum()
}


# Decode a text given a cipher
decode_text <- function(ciphered_text, cipher) {
  chartr(
    x = ciphered_text,
    old = paste(cipher, collapse = ""),
    new = paste(letters, collapse = "")
  )
}

# Define a function that randomly swaps out two letters in the string
swap <- function(x){
  # Select two distinct indices - this is the main engine that tests out messages
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}

mcmc_cipher <- function(input_text,tot_iter=1000,print_message = FALSE) {
  
  # Create another random cipher to be the starting cipher for the Markov Chain
  current_cipher <- sample(letters,
                           replace = FALSE)
  
  
  ciphered_text <- input_text
  # A counter to track how many decoded texts have been accepted
  i <- 0
  
  for (iter in 1:tot_iter) {
    
    # Propose a new cipher by swapping two letters in the current cipher
    proposed_cipher <- swap(current_cipher)
    
    # Text decoded from the proposal cipher
    decoded_text_proposed <- decode_text(ciphered_text,
                                         cipher = proposed_cipher)
    
    # Text decoded from the current cipher
    decoded_text_current <- decode_text(ciphered_text,
                                        cipher = current_cipher)
    
    # Log-likelihood of the decoded text from the proposal cipher
    proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
    
    # Log-likelihood of the decoded text from the current cipher
    current_log_lik <- get_log_lik_text(decoded_text_current)
    
    # Acceptance probability of the proposal, defined by the Metropolis algorithm
    # Remember that subtraction on the log-scale is division on the probability
    # scale. We exponentiate to get back to the probability scale.
    acceptance_probability <- min(1, exp(proposed_log_lik - current_log_lik))
    
    # Accept or not with probability given by `acceptance_probability`
    accept <- sample(c(TRUE, FALSE),
                     size=1,
                     prob = c(acceptance_probability,
                              1-acceptance_probability))
    
    # IF we accept the proposal, set the proposal cipher as the current cipher
    # ELSE, go on to the next iteration
    if (accept) {
      current_cipher <- proposed_cipher
      
      
      # Increment the counter so that we can keep track of acceptances
      # This is just for printing the output
      i <- i + 1
    }
    
    if (iter%%1000 == 0 & print_message) {
      # Print the text as decoded by the current cipher
      print(glue::glue("Iteration {iter}: {decoded_text_current}"))
    }
    
  }
  
  return(decoded_text_current)
  
}


# Use reduce with an initial value
final_result <- reduce(1:5, ~mcmc_cipher(.x,tot_iter = 1000,print_message = FALSE), .init = ciphered_text,)

plaintext <- "to be or not to be that is the question whether tis nobler in the mind to suffer the slings and arrows of outrageous fortune or to take arms against a sea of troubles"

# Set up the file for all the quotes and messages
quotes <- tibble(names = c("Saad",
                           "Vincent R.",
                           "Sean",
                           "Vincent G.",
                           "Muhayya",
                           "Portia",
                           "Hannah",
                           "Ivie",
                           "Arabella",
                           "Jess"),
                 messages = c(
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx",
                   "jv zt vf nvj jv zt jqdj lx jqt ustxjlvn hqtjqtf jlx nvzytf ln jqt alnc jv xsmmtf jqt xylnbx dnc dffvhx vm vsjfdbtvsx mvfjsnt vf jv jdit dfax dbdlnxj d xtd vm jfvszytx"                 )
)


outputs <- quotes |>
  mutate(decoded = map_chr(.x=messages,.f=~reduce(1:10, ~mcmc_cipher(.x,tot_iter=1000,print_message = FALSE), .init = .x),.progress=TRUE))
