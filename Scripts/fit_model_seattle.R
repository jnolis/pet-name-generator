source("Scripts/make_data.R")
source("Scripts/generator.R")

num_str_length <- 12


pet_data <- get_seattle_pet_data()


c(previous_letters_data, species_data, pet_subs) %<-% 
  split_into_subs_seattle(pet_data, character_lookup, species_lookup, num_str_length+1)

is_train <- runif(nrow(pet_subs)) < 0.9

train_y <- previous_letters_data[is_train,num_str_length+1,]
train_pl <- previous_letters_data[is_train,1:num_str_length,]
train_s <- species_data[is_train,]

test_y <- previous_letters_data[!is_train,num_str_length+1,]
test_pl <- previous_letters_data[!is_train,1:num_str_length,]
test_s <- species_data[!is_train,]


num_characters <- nrow(character_lookup)+1

species_input <- layer_input(shape = c(2), 
                            name = "species_input")
species_dense <- species_input %>% layer_dense(units=1,name="species_dense")
previous_letters_input <- 
  layer_input(shape = c(num_str_length,num_characters), name = "previous_letters_input") 
previous_letters_lstm <- 
  previous_letters_input %>%
  layer_lstm(input_shape = c(num_str_length,num_characters), units=64, name="previous_letters_lstm")

output <- 
  layer_concatenate(c(previous_letters_lstm, species_dense)) %>%
  layer_dropout(0.2) %>%
  layer_dense(64,name="joined_dense") %>%
  layer_dropout(0.2) %>%
  layer_dense(num_characters, name="reduce_to_characters_dense") %>%
  layer_activation("softmax", "final_activation")

model <- keras_model(inputs = c(previous_letters_input,species_input), outputs = output)


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.01),
  metrics = c('accuracy')
)

fit_results <- model %>% keras::fit(
  list(train_pl,train_s), list(train_y),
  batch_size = 64,
  epochs = 12,
  validation_data = list(list(test_pl,test_s), list(test_y))
 )

generated_names <- 1:100 %>%
  map(function(x){
    species <- species_lookup %>% pull(species) %>% sample(1)
    name <- generate_name_seattle(species, model, character_lookup, species_lookup, num_str_length, 0.7)
    list(species=species,name=name)
  }) %>%
  bind_rows() %>%
  anti_join(pet_data,by="name")

