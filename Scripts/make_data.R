#this file loads the packages and creates the functions that will be used in the model
require(tidyverse)
require(keras)
require(tidytext)
require(tokenizers)

character_lookup <-
  data_frame(character = c(LETTERS,".","-"," ","+")) %>% 
  mutate(character_id = row_number() - 1)

gender_lookup <- 
  data_frame(gender = c("F","M"), gender_id = c(0L,1L))

species_lookup <-
  data_frame(species = c("CAT","DOG"), species_id = c(0L, 1L))

# This function loads the raw text of the plates
get_nyc_pet_data <- function() {
  read_csv("Data/NYC_Dog_Licensing_Dataset.csv", col_types = cols(
    RowNumber = col_integer(),
    AnimalName = col_character(),
    AnimalGender = col_character(),
    AnimalBirthMonth = col_character(),
    BreedName = col_character(),
    Borough = col_character(),
    ZipCode = col_integer(),
    CommunityDistrict = col_integer(),
    CensusTract2010 = col_character(),
    NTA = col_character(),
    CityCouncilDistrict = col_integer(),
    CongressionalDistrict = col_integer(),
    StateSenatorialDistrict = col_integer(),
    LicenseIssuedDate = col_character(),
    LicenseExpiredDate = col_character()
  )) %>%
    transmute(name=AnimalName,gender=AnimalGender,breed=BreedName) %>%
    filter(!is.na(name),!is.na(gender),!is.na(breed)) %>%
    filter(!str_detect(name,"[^ \\.-[a-zA-Z]]")) %>%
    mutate(name = name %>% toupper()) %>%
    filter(name != "", name != "UNKNOWN", name != "NAME NOT PROVIDED") %>%
    mutate(id = row_number())
}

get_seattle_pet_data <- function(){
  read_csv("Data/Seattle_Pet_Licenses.csv", col_types = cols(
    `License Issue Date` = col_character(),
    `License Number` = col_integer(),
    `Animal's Name` = col_character(),
    Species = col_character(),
    `Primary Breed` = col_character(),
    `Secondary Breed` = col_character(),
    `ZIP Code` = col_character()
  )) %>%
    transmute(name = `Animal's Name`,
              species = `Species`,
              primary_breed = `Primary Breed`,
              secondary_breed = `Secondary Breed`) %>%
    mutate_all(toupper) %>%
    filter(!is.na(name),!is.na(species)) %>%
    filter(!str_detect(name,"[^ \\.-[a-zA-Z]]")) %>%
    mutate_all(toupper) %>%
    filter(name != "") %>%
    mutate(id = row_number())
}

get_breed_lookup <- function(pet_data){
  pet_data %>% 
    distinct(breed) %>% 
    mutate(breed_id = row_number()-1L)
}

add_stop <- function(plates, symbol="+") str_c(plates,symbol) # make a note for the end of a plate

# for each plate, we want to predict each of the n character on the plate. This we have to split one
# data point (a plate) into n data points (where n is the number of characters on the plate).
# So plate ABC would become data points "A", "AB", and "ABC"
split_into_subs_nyc <- function(pet_data, character_lookup, breed_lookup, gender_lookup, num_str_length){
  splits <- 
    pet_data %>% 
    pull(name) %>%
    add_stop() %>%
    tokenize_characters(lowercase=FALSE)
  subs <- map2(splits, pet_data$id, function(split, id){
    subs <- map(1:length(split),function(i) split[1:i])
    data_frame(id = id, sub = subs)
  }) %>%
    bind_rows() %>%
    mutate(sub_string = as_vector(map(sub,~ paste0(.x,collapse=""))),
           sub = sub %>% 
             map(~ character_lookup$character_id[match(.x,character_lookup$character)])) %>%
    inner_join(pet_data,.,by="id") %>%
    inner_join(breed_lookup,by="breed") %>%
    inner_join(gender_lookup,by="gender")
  breed_data <- to_categorical(subs$breed_id, num_classes = nrow(breed_lookup))

  gender_data <- to_categorical(subs$gender_id, num_classes = 2)
  fill_num <- nrow(character_lookup)
  previous_letters_data <- 
    subs$sub %>%
    pad_sequences(maxlen=num_str_length,value=nrow(character_lookup)) %>%
    to_categorical(num_classes = nrow(character_lookup) + 1)
  list(previous_letters_data,breed_data,gender_data,subs)
}

split_into_subs_seattle <- function(pet_data, character_lookup, species_lookup, num_str_length){
  splits <- 
    pet_data %>% 
    pull(name) %>%
    add_stop() %>%
    tokenize_characters(lowercase=FALSE)
  subs <- map2(splits, pet_data$id, function(split, id){
    subs <- map(1:length(split),function(i) split[1:i])
    data_frame(id = id, sub = subs)
  }) %>%
    bind_rows() %>%
    mutate(sub_string = as_vector(map(sub,~ paste0(.x,collapse=""))),
           sub = sub %>% 
             map(~ character_lookup$character_id[match(.x,character_lookup$character)])) %>%
    inner_join(pet_data,.,by="id") %>%
    inner_join(species_lookup,by="species")

  species_data <- to_categorical(subs$species_id, num_classes = 2)
  fill_num <- nrow(character_lookup)
  previous_letters_data <- 
    subs$sub %>%
    pad_sequences(maxlen=num_str_length,value=nrow(character_lookup)) %>%
    to_categorical(num_classes = nrow(character_lookup) + 1)
  list(previous_letters_data,species_data,subs)
}
