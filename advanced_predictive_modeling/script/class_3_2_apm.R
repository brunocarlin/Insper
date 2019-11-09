library(keras)

mnist <- dataset_mnist()


x_train <- mnist$train$x
y_train <- mnist$train$y

x_test <- mnist$test$x
y_test <- mnist$test$y

par(mfcol =c(6,12))
par(mar = c(0,0,2,0))

set.seed(1)

indices <- sample(nrow(x_train),72)

for (id in indices) {
  im <- x_train[id, ,]
  im <- t(apply(im,2,rev))
  image(
    1:28,
    1:28,
    im,
    col = grey((0:255) / 255),
    xaxt = "n",
    main = y_train[id]
  )
}



# array reshape ---------------------------------------------------------------------------------------------------

x <- matrix(1:16, nrow = 4)
array_reshape(x, c(1,16))


x_train <- array_reshape(x_train,c(nrow(x_train),28*28))

x_test <- array_reshape(x_test,c(nrow(x_test),28*28))

dim(x_train)


x_train <- x_train/255
x_test  <- x_test/255

y_test <- y_test %>% to_categorical()
y_train <- y_train %>% to_categorical()


use_session_with_seed(1234,
                      disable_parallel_cpu = FALSE,
                      disable_gpu = TRUE)

batch_size <- 128
num_classes <- 10
epochs <- 12

# input layer
inputs <- layer_input(shape = c(784))

# outputs compose input + dense layers
predictions <- inputs %>%
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dropout(.2) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(.2) %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# create and compile model
model1 <- keras_model(inputs = inputs, outputs = predictions)

model1 %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)



# fit model -------------------------------------------------------------------------------------------------------
model1 %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = 2,
  validation_split = 0.2
)


# predict and evaluate model --------------------------------------------------------------------------------------

scores <- model %>% evaluate(
  x_test, y_test, verbose = 0
)

model1 %>% predict(x_test)
pred <- predict(object = model1,x = x_test)

pred_classes <- pred %>% apply(1, which.max) -1


stest <- which(pred_classes != mnist$test$y)

set.seed(1)
indices <- sample(stest, 64)

par(mfcol = c(8,8))
par(mar = c(0,0,2,0))

for(id in indices){
  im <- mnist$test$x[id, ,]
  im <- t(apply(im, 2, rev))
  im <- (im[1:28,1:28]-255)*(-1)
  image(1:28, 1:28, im, col = grey((0:255)/255),
        xaxt = "n", yaxt = "n", 
        main = paste(pred_classes[id], "vs", mnist$test$y[id]))
}
