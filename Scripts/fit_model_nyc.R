source("Scripts/make_data.R")
source("Scripts/generator.R")

num_str_length <- 12

fix_breed_dim <- function(breed_data){
  dim(breed_data) <- c(dim(breed_data)[1],1,dim(breed_data)[2])
  breed_data
}

pet_data <- get_nyc_pet_data()

breed_lookup <- get_breed_lookup(pet_data)

c(previous_letters_data,breed_data,gender_data, pet_subs) %<-% 
  split_into_subs_nyc(pet_data, character_lookup, breed_lookup, gender_lookup, num_str_length+1)

is_train <- runif(nrow(pet_subs)) < 0.9

train_y <- previous_letters_data[is_train,num_str_length+1,]
train_pl <- previous_letters_data[is_train,1:num_str_length,]
train_b <- breed_data[is_train,]
train_g <- gender_data[is_train,]

test_y <- previous_letters_data[!is_train,num_str_length+1,]
test_pl <- previous_letters_data[!is_train,1:num_str_length,]
test_b <- breed_data[!is_train,]
test_g <- gender_data[!is_train,]

num_breeds <- nrow(breed_lookup)
num_characters <- nrow(character_lookup)+1

breed_input <- layer_input(shape = c(num_breeds), name = "breed_input")

breed_dense <- 
  breed_input %>% 
  layer_dense(units=8,name="breed_dense")

gender_input <- layer_input(shape = c(2), 
                            name = "gender_input")
gender_dense <- gender_input %>% layer_dense(units=1,name="gender_dense")
previous_letters_input <- 
  layer_input(shape = c(num_str_length,num_characters), name = "previous_letters_input") 
previous_letters_lstm <- 
  previous_letters_input %>%
  layer_lstm(input_shape = c(num_str_length,num_characters), units=64, name="previous_letters_lstm")

output <- 
  layer_concatenate(c(previous_letters_lstm, gender_dense, breed_dense)) %>%
  layer_dropout(0.2) %>%
  layer_dense(64,name="joined_dense") %>%
  layer_dropout(0.2) %>%
  layer_dense(num_characters, name="reduce_to_characters_dense") %>%
  layer_activation("softmax", "final_activation")

model <- keras_model(inputs = c(previous_letters_input,gender_input,breed_input), outputs = output)


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.01),
  metrics = c('accuracy')
)

fit_results <- model %>% keras::fit(
  list(train_pl,train_g,train_b), list(train_y),
  batch_size = 64,
  epochs = 12,
  validation_data = list(list(test_pl,test_g,test_b), list(test_y))
 )

generated_names <- 1:100 %>%
  map(function(x){
    gender <- sample(c("M","F"), 1)
    breed <- breed_lookup %>% sample_n(1) %>% pull(breed)
    name <- generate_name(gender, breed ,
                          model, character_lookup, breed_lookup, gender_lookup, num_str_length, 0.9)
    list(gender=gender,breed=breed,name=name)
  }) %>%
  bind_rows() %>%
  anti_join(pet_data,by="name")

