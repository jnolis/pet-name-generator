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
  split_into_subs(pet_data, character_lookup, breed_lookup, gender_lookup, num_str_length+1)

is_train <- runif(nrow(pet_subs)) < 0.9

train_y <- previous_letters_data[is_train,num_str_length+1,]
train_pl <- previous_letters_data[is_train,1:num_str_length,]

test_y <- previous_letters_data[!is_train,num_str_length+1,]
test_pl <- previous_letters_data[!is_train,1:num_str_length,]

num_characters <- nrow(character_lookup)+1

previous_letters_input <- 
  layer_input(shape = c(num_str_length,num_characters), name = "previous_letters_input") 
output <- 
  previous_letters_input %>%
  layer_lstm(input_shape = c(num_str_length,num_characters), units=64, name="previous_letters_lstm") %>%
  layer_dense(num_characters, name="reduce_to_characters_dense") %>%
  layer_activation("softmax", "final_activation")

model <- keras_model(inputs = c(previous_letters_input), outputs = output)


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.01),
  metrics = c('accuracy')
)

fit_results <- model %>% keras::fit(
  list(train_pl), list(train_y),
  batch_size = 64,
  epochs = 12,
  validation_data = list(list(test_pl), list(test_y))
 )

generated_names <- seq(0.4,1.0,length.out=500) %>%
  map(function(x){
    name <- generate_name_only(model, character_lookup, num_str_length, x)
    list(name=name, temp = x)
  }) %>%
  bind_rows() %>%
  anti_join(pet_data,by="name")

View(generated_names)

write_csv(generated_names,"generated_names.csv")
