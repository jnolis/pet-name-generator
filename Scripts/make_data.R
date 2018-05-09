#this file loads the packages and creates the functions that will be used in the model
require(tidyverse)
require(keras)
require(tidytext)
require(tokenizers)
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
    mutate(name = str_remove_all(name,"[^ \\.-[:alpha:]]")) %>%
    filter(name != "") %>%
    mutate(id = row_number())
}

add_stop <- function(plates, symbol="+") str_c(plates,symbol) # make a note for the end of a plate

# for each plate, we want to predict each of the n character on the plate. This we have to split one
# data point (a plate) into n data points (where n is the number of characters on the plate).
# So plate ABC would become data points "A", "AB", and "ABC"
split_into_subs <- function(pet_data){
    splits <- 
      pet_data %>% 
      pull(name) %>%
      add_stop() %>%
      tokenize_characters(lowercase=FALSE)
    subs <- map2(splits, pet_data$id, function(split, id){
        subs <- map(1:length(split),function(i) split[1:i])
        data_frame(id = id, sub = subs)
      }) %>%
    bind_rows()
  pet_data %>% inner_join(subs,by="id")
}

