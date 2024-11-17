### MAT 373, Day 33
### Part 1: Create a cipher to code a message

# Define encode/decode functions:
# (1) encode using the cipher
# (2) decode using the cipher

# Encode a text using a cipher
encode_text <- function(text, cipher) {
  chartr(
    x = text,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}

# Decode a text given a cipher
decode_text <- function(ciphered_text, cipher) {
  chartr(
    x = ciphered_text,
    old = paste(cipher, collapse = ""),
    new = paste(letters, collapse = "")
  )
}

# Writing a coded message is a two step process:
# (1) generate the "true" cipher
# (2) Generate a random cipher to be the true cipher

# define the message you want to encode (find a famous quote): https://www.goodreads.com/quotes
plaintext = "Some string, can include puncutation."

# Generate a new cipher by permuting the letters of the alphabet
true_cipher <- sample(letters,
                      replace = FALSE)

# Encode the plaintext
message <- encode_text(text = plaintext,
                       cipher = true_cipher)

# Send me your ciphered_text (paste into an email and send)

# Check to see if your message decode correctly:
decode_text(ciphered_text = message,
            cipher = true_cipher)
