library("keras")
library("dplyr")

# Defina o caminho do diretório de treinamento
dataset_training <- "image\\images_train"

taille_cible <- c(32, 32)
taille_lot <- 32

train_data_gen <- image_data_generator(
  rescale = 1 / 255,
  horizontal_flip = TRUE,
  vertical_flip = TRUE,
  rotation_range = 45,
  zoom_range = 0.25,
  validation_split = 0.2
)

# Jeu de données d'entraînement
train_image_array_gen <- flow_images_from_directory(
  directory = dataset_training,
  target_size = taille_cible,
  color_mode = "rgb",
  batch_size = taille_lot,
  seed = 123,
  subset = "training",
  generator = train_data_gen,
  class_mode = "sparse"
)

# Jeu de données de validation
val_image_array_gen <- flow_images_from_directory(
  directory = dataset_training,
  target_size = taille_cible,
  color_mode = "rgb",
  batch_size = taille_lot,
  seed = 123,
  subset = "validation",
  generator = train_data_gen,
  class_mode = "sparse"
)

# Nombre d'échantillons d'entraînement
train_samples <- train_image_array_gen$n

# Nombre d'échantillons de validation
valid_samples <- val_image_array_gen$n

# Nombre de classes/catégories cibles library - dplyr
output_n <- n_distinct(train_image_array_gen$classes)

model <- keras_model_sequential(name = "simple_model") %>%
  # Couche de convolution
  layer_conv_2d(
    filters = 16,
    kernel_size = c(3, 3),
    padding = "same",
    activation = "relu",
    input_shape = c(taille_cible, 3)
  ) %>%

  # Couche de pooling max
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  # Couche d'aplatissement
  layer_flatten() %>%
  # Couche dense
  layer_dense(units = 16, activation = "relu") %>%
  # Couche de sortie
  layer_dense(
    units = output_n,
    activation = "softmax",
    name = "Sortie"
  )

model %>%
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = optimizer_adam(lr = 0.01),
    metrics = "accuracy"
  )

# Ajuster les données dans le modèle
history <- model %>%
  fit(
    # données d'entraînement
    train_image_array_gen,
    # époques d'entraînement
    steps_per_epoch = as.integer(train_samples / taille_lot),
    epochs = 50,
    # données de validation
    validation_data = val_image_array_gen,
    validation_steps = as.integer(valid_samples / taille_lot)
  )

# Salvar o modelo para submissão
model %>% save_model_tf("my_model.keras")
