choose_next_char <- function(preds, character_lookup,temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  next_index <- 
    rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
  character_lookup$character[next_index]
}

generate_name_nyc <- function(gender_value, breed_value, model, character_lookup, breed_lookup, gender_lookup, num_str_length,temperature=1){
  continue <- TRUE
  sentence <- character()
  gender_data <- gender_lookup %>% filter(gender==gender_value) %>% pull(gender_id) %>% 
    to_categorical(nrow(gender_lookup)) %>%
    array(dim=c(1,nrow(gender_lookup)))
  breed_data <- 
    breed_lookup %>% 
    filter(breed==breed_value) %>% 
    pull(breed_id) %>% 
    to_categorical(nrow(breed_lookup)) %>%
    array(dim=c(1,nrow(breed_lookup)))
  previous_letters_data <- 
    rep(nrow(character_lookup), num_str_length) %>% 
    to_categorical(nrow(character_lookup)+1) %>%
    array(dim=c(1,num_str_length,nrow(character_lookup)+1))
  
  while(continue){
    next_letter_probabilities <- 
      predict(model,list(previous_letters_data,gender_data,breed_data))
    next_letter <- choose_next_char(next_letter_probabilities,character_lookup,temperature)
    if(next_letter == "+" || length(sentence) > 30){
      continue <- FALSE
    } else {
      sentence <- c(sentence,next_letter)
      previous_letters_data <- 
        character_lookup$character_id[match(sentence,character_lookup$character)] %>%
        list() %>%
        pad_sequences(maxlen=12,value=nrow(character_lookup)) %>% 
        to_categorical(nrow(character_lookup)+1)
    }
  }
  sentence %>%
    paste0(collapse="")
}

generate_name_seattle<- function(species_value, model, character_lookup, species_lookup, num_str_length,temperature=1){
  continue <- TRUE
  sentence <- character()
  species_data <- species_lookup %>% filter(species==species_value) %>% pull(species_id) %>% 
    to_categorical(nrow(species_lookup)) %>%
    array(dim=c(1,nrow(species_lookup)))

  previous_letters_data <- 
    rep(nrow(character_lookup), num_str_length) %>% 
    to_categorical(nrow(character_lookup)+1) %>%
    array(dim=c(1,num_str_length,nrow(character_lookup)+1))
  
  while(continue){
    next_letter_probabilities <- 
      predict(model,list(previous_letters_data,species_data))
    next_letter <- choose_next_char(next_letter_probabilities,character_lookup,temperature)
    if(next_letter == "+" || length(sentence) > 30){
      continue <- FALSE
    } else {
      sentence <- c(sentence,next_letter)
      previous_letters_data <- 
        character_lookup$character_id[match(sentence,character_lookup$character)] %>%
        list() %>%
        pad_sequences(maxlen=12,value=nrow(character_lookup)) %>% 
        to_categorical(nrow(character_lookup)+1)
    }
  }
  sentence %>%
    paste0(collapse="")
}

choose_next_char <- function(preds, character_lookup,temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  next_index <- 
    rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
  character_lookup$character[next_index]
}

generate_name_only <- function( model, character_lookup,  num_str_length,temperature=1){
  
  
  continue <- TRUE
  sentence <- character()

  previous_letters_data <- 
    rep(nrow(character_lookup), num_str_length) %>% 
    to_categorical(nrow(character_lookup)+1) %>%
    array(dim=c(1,num_str_length,nrow(character_lookup)+1))
  
  while(continue){
    next_letter_probabilities <- 
      predict(model,list(previous_letters_data))
    next_letter <- choose_next_char(next_letter_probabilities,character_lookup,temperature)
    if(next_letter == "+" || length(sentence) > 30){
      continue <- FALSE
    } else {
      sentence <- c(sentence,next_letter)
      previous_letters_data <- 
        character_lookup$character_id[match(sentence,character_lookup$character)] %>%
        list() %>%
        pad_sequences(maxlen=12,value=nrow(character_lookup)) %>% 
        to_categorical(nrow(character_lookup)+1)
    }
  }
  sentence %>%
    paste0(collapse="")
}
# 
# 
# species_guess_seattle<- function(pet_name, species_value, model, character_lookup, species_lookup, num_str_length,temperature=1){
#   c(previous_letters_data,species_data,subs) %<-% 
#     data_frame(name=pet_name, species = species_value) <-
#     split_into_subs_seattle(character_lookup, species_lookup, num_str_length)
#   
#   previous_letters_data <- 
#     rep(nrow(character_lookup), num_str_length) %>% 
#     to_categorical(nrow(character_lookup)+1) %>%
#     array(dim=c(1,num_str_length,nrow(character_lookup)+1))
#   
#   while(continue){
#     next_letter_probabilities <- 
#       predict(model,list(previous_letters_data,species_data))
#     next_letter <- choose_next_char(next_letter_probabilities,character_lookup,temperature)
#     if(next_letter == "+" || length(sentence) > 30){
#       continue <- FALSE
#     } else {
#       sentence <- c(sentence,next_letter)
#       previous_letters_data <- 
#         character_lookup$character_id[match(sentence,character_lookup$character)] %>%
#         list() %>%
#         pad_sequences(maxlen=12,value=nrow(character_lookup)) %>% 
#         to_categorical(nrow(character_lookup)+1)
#     }
#   }
#   sentence %>%
#     paste0(collapse="")
# }