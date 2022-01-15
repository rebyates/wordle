library(tidyverse)

words <- words::words$word

# Wordle words are 5 letters long
words_5 <- words[nchar(words) == 5]

words_5

# List of most commonly used to least commonly used letters in 5 letter words
letter_count <- words_5 %>% 
  paste(collapse="") %>%
  str_split(pattern="") %>%
  table() %>%
  as.data.frame()

names(letter_count) <- c('word', 'word_count')

top_letters <- letter_count %>%
  arrange(desc(word_count)) %>%
  head(5) %>%
  select(word) %>%
  pull()

top_letters <- as.character(top_letters)

# Find 'best' first word guess using most commonly used letters
words_containing_letter <- function(words, letter){
  words[str_detect(words, letter)]
}

words_5_with_top_letters = c()
for (letter in top_letters){
  words_5_with_top_letters <- c(words_5_with_top_letters,
                                words_containing_letter(words_5, letter=letter))
}
  
as.data.frame(x=words_5_with_top_letters) %>%
  rename(word = words_5_with_top_letters) %>%
  group_by(word) %>%
  summarise(contains_top_letter = n()) %>%
  arrange(desc(contains_top_letter)) %>% View()
# The word 'arose' is the only word in the words dictionary that contains the 5 most commonly used letters
# This is likely to be a good first pick
# Future improvements could reduce the weight of 's' in the dictionary because of its use in plurals


# Input guess
# Input GREEN, YELLOW, GREY from guess
# Output new guess

# If there are any greys, limit the words_5 to contain words without those letters
# If there are any yellows, limit words_5 to contain words with those letters
# If there are any greens, limits words_5 to contain those letters in those positions

# First guess in BANDA
# YELLOW, GREEN, GREY, GREY, GREY

1
words_5 <- words[nchar(words) == 5]
first_guess <- c('banda')
colours <- c('GREY', 'GREEN', 'GREEN', 'GREY', 'YELLOW')

first_guess_split <- str_split(first_guess, "")[[1]] 

grey_index <- colours == 'GREY'
grey_letters <- first_guess_split[grey_index]

for (letter in grey_letters){
  words_5 <- words_5[!str_detect(words_5,letter)]
}

yellow_index <- colours == 'YELLOW'
yellow_letters <- first_guess_split[yellow_index]

for (letter in yellow_letters){
  words_5 <- words_5[str_detect(words_5,letter)]
}

green_index <- colours == 'GREEN'
green_pattern = c('.','.','.','.','.')

for (position in c(1:5)){
  if (green_index[position]){
    green_pattern[position] = first_guess_split[position]
  }
}

green_pattern <- paste(green_pattern, collapse="")
words_5 <- words_5[str_detect(words_5,green_pattern)]
words_5

words_5 %>% sample(1) # next guess
