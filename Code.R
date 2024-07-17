# ------------------------------------------------------------------------------
# - SISTEMA DE RECOMENDACIÓN DE PELÍCULAS BASADO EN ITEMS Y BASADO EN USUARIOS -
# ------------------------------------------------------------------------------

# Instalar paqueterías para el procesado y manipulación de información ---------
# install.packages("ROCR")
# install.packages("jsonlite")
# install.packages("recommenderlab")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("reshape2")
# install.packages("GGally")
# install.packages("corrplot")
# install.packages("PerformanceAnalytics")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("shiny")

library(ROCR)
library(jsonlite)
library(recommenderlab)
library(dplyr)
library(tidyr)
library(reshape2)
library(GGally)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(ggthemes)
library(shiny)

# Lectura de la data -----------------------------------------------------------
path <- ("/Users/abrahamsandovalaltamirano/Library/CloudStorage/OneDrive-UniversidadAutónomadelEstadodeMéxico/Notion Links/Studies Dash/Master/Tests/4. Proyecto Final de Maestría/Movies Project/Master")
df_movies <- fromJSON(paste0(path, "/Movies.json"))$data
df_reviews <- fromJSON(paste0(path, "/Reviews.json"))$data

paste0("El dataframe de Movies tiene un total de: ", nrow(df_movies), " filas, con: ", ncol(df_movies), " atributos o variables, las cuales son: ", list(colnames(df_movies)))
paste0("El dataframe de Reviews tiene un total de: ", nrow(df_reviews), " filas, con: ", ncol(df_reviews), " atributos o variables, las cuales son: ", list(colnames(df_reviews)))

df_final <- merge(df_reviews, df_movies, by.x = "movieID", by.y = "id")
df_cust <- df_final %>% select(title, movieID, author, genre_ids, overview, popularity, vote_average, vote_count)

glimpse(df_cust)
summary(df_cust)

paste0("El dataframe contiene: ", length(unique(df_cust$title)), " películas únicas, así como: ", length(unique(df_cust$author)), " usuarios únicos. Lo cuál da un promedio de: ", round(length(unique(df_cust$title))/length(unique(df_cust$author)), 0), " valoraciones por usuario")

# Ajuste por usuario y película para obtener promedio mayor de valoraciones ----
set.seed(15)
samp_mov <- sample(unique(df_cust$title), 150)

rep_mov <- rep(samp_mov, each = 90)
need <- nrow(df_cust) - length(rep_mov)
rep_mov <- c(rep_mov, sample(samp_mov, need, replace = TRUE))
rep_mov <- sample(rep_mov)

df_cust$title <- rep_mov

set.seed(15)
samp_aut <- sample(unique(df_cust$author), 200)

rep_aut <- rep(samp_aut, each = 60)
need <- nrow(df_cust) - length(rep_aut)
rep_aut <- c(rep_aut, sample(samp_aut, need, replace = TRUE))
rep_aut <- sample(rep_aut)

df_cust$author <- rep_aut

paste0("El dataframe modificado contiene: ", length(unique(df_cust$title)), " películas únicas, así como: ", length(unique(df_cust$author)), " usuarios únicos. El promedio de valoraciones por usuario es de: ", round(length(df_cust$title)/length(unique(df_cust$author)), 0))

(histograma <- ggplot(df_cust, aes(x = vote_average)) +
  ggtitle("Calificaciones por película") +
  theme_fivethirtyeight() +
  geom_histogram(color = "#28324a", fill = "#3c78d8"))

chart.Correlation(df_cust[, c(2,6,7,8)], histogram = F, pch = 19)

# Convertir lista de géneros en columnas del data frame ------------------------
max(lengths(df_cust$genre_ids))

df_cust$genre_ids <- sapply(df_cust$genre_ids, paste, collapse = ",")
df_cust <- separate(df_cust, col = genre_ids, into = c("G1", "G2", "G3", "G4", "G5", "G6", "G7"), sep = ",", fill = "right", extra = "drop")

df_cust$G1[is.na(df_cust$G1)] <- 0
df_cust$G2[is.na(df_cust$G2)] <- 0
df_cust$G3[is.na(df_cust$G3)] <- 0
df_cust$G4[is.na(df_cust$G4)] <- 0
df_cust$G5[is.na(df_cust$G5)] <- 0
df_cust$G6[is.na(df_cust$G6)] <- 0
df_cust$G7[is.na(df_cust$G7)] <- 0

df_fin <- df_cust %>% mutate_at(vars(G1, G2, G3, G4, G5, G6, G7), as.integer)
summary(df_fin)

# Convertir data frame a matriz de calificaciones ------------------------------
df_mtx <- df_fin %>% ungroup() %>% select(author, title, vote_average)
mtx <- acast(df_mtx, author ~ title, fun.aggregate = mean, value.var = "vote_average")
mtx <- round(mtx, 0)
mtx_fin <- as(mtx, "realRatingMatrix")
summary(mtx_fin)

head(rownames(mtx_fin))
head(colnames(mtx_fin))
mtx_fin@data[1,]

# Creación de los conjuntos de entrenamiento y de test -------------------------
t.id <- sample(x = c(T, F), size = nrow(mtx_fin), replace = T, prob = c(0.8, 0.2))

data_train <- mtx_fin[t.id, ]
data_test <- mtx_fin[!t.id, ]

# TIpos de modelos usados para trabajar las real rating matrix y generar sitemas de recomendaciones
# Para este caso se usan Items y Users
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)

# Representacion de la matriz de recomendaciones -------------------------------
image(mtx_fin, main = "Mapa de calor de la matriz de valoraciones de películas")

min_r_movies <- quantile(rowCounts(mtx_fin), 0.85)
min_r_users <- quantile(colCounts(mtx_fin), 0.85)
image(mtx_fin[rowCounts(mtx_fin)> min_r_movies, colCounts(mtx_fin)>min_r_users], main = "Mapa de calor del top de películas y usuarios")

# ------------------------------------------------------------------------------
# Filtrado colaborativo basado en los ítems (IBCF)
ibcf <- Recommender(data = data_train, method = "IBCF", parameter = list(k = 30))
ibcf.mod <- getModel(ibcf)
ibcf.mod

n_rec <- 10
ibcf.pred <- predict(object = ibcf, newdata = data_test, n = n_rec)
ibcf.pred

ibcf.rec.matrix <- sapply(ibcf.pred@items, function(x){ colnames(mtx_fin)[x]})

users <- unlist(data_test@data@Dimnames[1])
rn <- sapply(1:10, function(i) paste("Rec", i, "- IBCF"))
recomm <- as.data.frame(ibcf.rec.matrix)

colnames(recomm) <- users
rownames(recomm) <- rn

# ------------------------------------------------------------------------------
# Filtrado colaborativo basado en usuarios (UBCF)
ubcf <- Recommender(data = data_train, method = "UBCF")
ubcf.mod <- getModel(ubcf)
ubcf.mod

n_rec <- 10
ubcf.pred <- predict(object = ubcf, newdata = data_test, n = n_rec)
ubcf.pred

ubcf.rec.matrix <- sapply(ubcf.pred@items, function(x){colnames(mtx_fin)[x]})

users <- unlist(data_test@data@Dimnames[1])
rn <- sapply(1:10, function(i) paste("Rec", i, "- UBCF"))
reco <- as.data.frame(ubcf.rec.matrix)

colnames(reco) <- users
rownames(reco) <- rn

vv <- rep("-", 36)
br <- data.frame(matrix(vv, nrow = 1, ncol = 36))
colnames(br) <- users
rownames(br) <- "-"

df1 <- rbind(recomm, br)
recommendations <- rbind(df1, reco)

# ------------------------------------------------------------------------------
# Sistemas de recomendación híbridos
hybrid_recom <- HybridRecommender(
  Recommender(data_train, method = "UBCF"),
  Recommender(data_train, method = "RANDOM"), 
  weights = c(0.25, 0.75)
)

hyb_recom <- as.data.frame(as(predict(hybrid_recom, data_test, 10), "list"))
colnames(hyb_recom) <- users

rn <- sapply(1:10, function(i) paste("Rec", i, "- Hybrid"))
hr <- as.data.frame(hyb_recom)

colnames(hyb_recom) <- users
rownames(hyb_recom) <- rn

vv <- rep("-", 36)
br2 <- data.frame(matrix(vv, nrow = 1, ncol = 36))
colnames(br2) <- users
rownames(br2) <- "-"

df11 <- rbind(recommendations, br2)
recommendations <- rbind(df11, hyb_recom)

# ------------------------------------------------------------------------------
# Recomendaciones basadas en datos binarios
mtx_fin_viewed <- binarize(mtx_fin, minRating = 1)
image(mtx_fin_viewed)

set.seed(15)
t.id <- sample(x = c(T, F), size = nrow(mtx_fin_viewed), replace = T, prob = c(0.8, 0.2))
b_data_train <- mtx_fin_viewed[t.id,]
b_data_test <- mtx_fin_viewed[!t.id,]

b_model <- Recommender(data = b_data_train, method = "IBCF", parameter = list(method = "Jaccard"))
getModel(b_model)

n_rec <- 10
b_pred <- predict(object = b_model, newdata = b_data_test, n = n_rec)
b_rec_matrix <- sapply(b_pred@items, function(x){colnames(mtx_fin)[x]})

# Filtro basado en contendio ---------------------------------------------------
df_c <- df_fin %>%
  select(title, G1, G2, G3, G4, G5, G6, G7)

unique_genres <- sort(unique(unlist(df_c[, c("G1", "G2", "G3", "G4", "G5", "G6", "G7")])))
gen <- c("Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

dict <- data.frame(unique_genres, gen)

df_c <- df_final %>%
  select(movieID, title, genre_ids)

for (genre in unique_genres) {
  df_c[[as.character(genre)]] <- 0
}

df_x <- df_c %>%
  rowwise() %>%
  mutate(across(all_of(as.character(unique_genres)), 
                ~ ifelse(as.numeric(cur_column()) %in% unlist(genre_ids), 1, 0), 
                .names = "{col}"))

df_x <- df_x[, -3]

new_row <- data.frame(unique_genres = c(999, 998), gen = c("MovieID", "Title"))
dict <- rbind(new_row, dict)
colnames(df_x) <- dict$gen
df_mov <- unique(df_x)

df_user <- df_final %>%
  select(author, movieID)

df_rate <- df_final$author_details
df_rate <- df_rate %>% select(rating)
df_usr <- cbind(df_user, df_rate)
df_usr <- unique(df_usr)
df_usr$UserID <- seq(1, nrow(df_usr))

df_usr <- df_usr %>%
  select(UserID, author, movieID, rating)

colnames(df_usr) <- c("UserID", "UserName", "MovieID", "rating")

clusterMovies <- function(df, kclust = 10){
  set.seed(1596)
  df <- df[, c(-1,-2)]
  mclust <- kmeans(df, centers = kclust, nstart = 20)
  return(mclust)
}

getUserInfo <- function(df, id){
  myUser <- subset(df, UserID == id, select = c(MovieID, rating))
  cluster <- 0
  activeUser <- data.frame(myUser[order(myUser$MovieID),], cluster)
  return(activeUser)
}

setUserMovieCluster <- function(m_title_df, mclust, activeUser){
  df_aux <- data.frame(cbind(m_title_df$MovieID, 
                             clustNum = mclust$cluster))
  names(df_aux) <- c("MovieID", "Cluster")
  activeUser$cluster <- df_aux[match(activeUser$MovieID, df_aux$MovieID), 2]
  return(activeUser)
}

getAverageClusterRating <- function(mclust, activeUser, minRating = 3){
  like <- aggregate(activeUser$rating, 
                    by = list(cluster = activeUser$cluster), mean)
  if(max(like$x) < minRating){
    like <- as.vector(0)
  } else {
    like <- as.vector(t(max(subset(like, x >= minRating, select = cluster))))
  }
  return(like)
}

getRecommendedMovies <- function(like, mclust, m_title_df){
  df_aux <- data.frame(cbind(m_title_df$MovieID, 
                             clusterNum = mclust$cluster))
  names(df_aux) = c("MovieID", "Cluster")
  if(like == 0){
    recommend <- m_title_df[sample.int(n = nrow(m_title_df), size = 100), 1]
  } else {
    recommend <- as.vector(t(subset(df_aux, Cluster == like, select = MovieID)))
  }
}

getRecommendations <- function(movie_df, user_df, userID){
  mclust <- clusterMovies(movie_df)
  activeUser <- getUserInfo(user_df, userID)
  activeUser <- setUserMovieCluster(movie_df, mclust, activeUser)
  like <- getAverageClusterRating(mclust, activeUser)
  recomendation <- getRecommendedMovies(like, mclust, movie_df)
  recomendation <- recomendation[-activeUser$MovieID]
  movieTitle <- movie_df[match(recomendation, movie_df$MovieID),2]
  recomendation <- data.frame(recomendation, movieTitle)
  return(recomendation)
}

suggestMovies <- function(movie_df, user_df, user_id, num_movies){
  suggestions <- getRecommendations(movie_df, user_df, user_id)
  suggestions <- suggestions[1:num_movies,]
  writeLines("Tal vez te gustaría ver también las siguientes películas:")
  write.table(suggestions[2], row.names = F, col.names = F)
}

suggestMovies(df_mov, df_usr, 69, 10)

# Validación del Modelo --------------------------------------------------------
df_custom <- df_cust %>% select(author, movieID, vote_average)

ratings_matrix <- as(df_custom, "realRatingMatrix")
recommender_model <- Recommender(ratings_matrix, method = "UBCF")
set.seed(15)
e <- evaluationScheme(ratings_matrix, method = "split", train = 0.8, given = -5, goodRating = 4)

recommender <- Recommender(getData(e, "train"), method = "UBCF")
predictions <- predict(recommender, getData(e, "known"), type = "ratings")
accuracy <- calcPredictionAccuracy(predictions, getData(e, "unknown"))
print(accuracy)

# Recolección de datos por parte del usuario -----------------------------------
df_mov_raw <- df_final %>%
  select(movieID, title, genre_ids)

for (genre in unique_genres) {
  df_c[[as.character(genre)]] <- 0
}

df_x <- df_c %>%
  rowwise() %>%
  mutate(across(all_of(as.character(unique_genres)), 
                ~ ifelse(as.numeric(cur_column()) %in% unlist(genre_ids), 1, 0), 
                .names = "{col}"))

df_x <- df_x[, -3]

new_row <- data.frame(unique_genres = c(999, 998), gen = c("MovieID", "Title"))
dict <- rbind(new_row, dict)
colnames(df_x) <- dict$gen
df_mov <- unique(df_x)

# App de solicitud de información al usuario
ui <- fluidPage(
  titlePanel("Ingreso de Películas"),
  sidebarLayout(
    sidebarPanel(
      # Campos de entrada
      textInput("titulo", "Título de la película:"),
      selectInput("genero", "Género de la película:", choices = dict$gen, selected = NULL),
      numericInput("puntuacion", "Puntuación de la película (0-10):", value = 0, min = 0, max = 10, step = 0.1),
      textInput("username", "Nombre del usuario:"),
      actionButton("agregar", "Agregar Película"),
      actionButton("salir", "Salir")
    ),
    mainPanel(
      tableOutput("tabla_peliculas")
    )
  )
)

server <- function(input, output, session) {
  next_movieID <- max(df_mov_raw$movieID, na.rm = TRUE) + 1
  next_UserID <- max(df_usr$UserID, na.rm = TRUE) + 1
  peliculas_reactivo <- reactiveVal(df_mov_raw)
  
  observeEvent(input$agregar, {
    genero_id <- dict$unique_genres[dict$gen == input$genero]
    
    # Añadir al dataframe movie_raw
    nueva_pelicula <- data.frame(
      movieID = next_movieID,
      title = input$titulo,
      genre_ids = genero_id,
      stringsAsFactors = FALSE
    )
    
    peliculas_actualizado <- rbind(peliculas_reactivo(), nueva_pelicula)
    peliculas_reactivo(peliculas_actualizado)
    
    df_mov_raw <<- peliculas_actualizado
    
    # Añadir al dataframe usr
    nuevo_usuario <- data.frame(
      UserID = next_UserID,
      UserName = input$username,
      MovieID = next_movieID,
      rating = input$puntuacion,
      stringsAsFactors = FALSE
    )
    
    usuarios_actualizado <- rbind(df_usr, nuevo_usuario)
    df_usr <<- usuarios_actualizado
    
    next_movieID <<- next_movieID + 1
    next_UserID <<- next_UserID + 1
  })
  
  observeEvent(input$salir, {
    stopApp()
  })
  
}

shinyApp(ui = ui, server = server)

df_c <- df_mov_raw %>%
  select(movieID, title, genre_ids)

for (genre in unique_genres) {
  df_c[[as.character(genre)]] <- 0
}

df_x <- df_c %>%
  rowwise() %>%
  mutate(across(all_of(as.character(unique_genres)), 
                ~ ifelse(as.numeric(cur_column()) %in% unlist(genre_ids), 1, 0), 
                .names = "{col}"))

df_x <- df_x[, -3]

new_row <- data.frame(unique_genres = c(999, 998), gen = c("MovieID", "Title"))
dict <- rbind(new_row, dict)
colnames(df_x) <- dict$gen
df_mov <- unique(df_x)

# Recomendaciones para el nuevo usuario ----------------------------------------
clusterMovies <- function(df, kclust = 10){
  set.seed(1596)
  df <- df[, c(-1,-2)]
  mclust <- kmeans(df, centers = kclust, nstart = 20)
  return(mclust)
}

getUserInfo <- function(df, id){
  myUser <- subset(df, UserID == id, select = c(MovieID, rating))
  cluster <- 0
  activeUser <- data.frame(myUser[order(myUser$MovieID),], cluster)
  return(activeUser)
}

setUserMovieCluster <- function(m_title_df, mclust, activeUser){
  df_aux <- data.frame(cbind(m_title_df$MovieID, 
                             clustNum = mclust$cluster))
  names(df_aux) <- c("MovieID", "Cluster")
  activeUser$cluster <- df_aux[match(activeUser$MovieID, df_aux$MovieID), 2]
  return(activeUser)
}

getAverageClusterRating <- function(mclust, activeUser, minRating = 3){
  like <- aggregate(activeUser$rating, 
                    by = list(cluster = activeUser$cluster), mean)
  if(max(like$x) < minRating){
    like <- as.vector(0)
  } else {
    like <- as.vector(t(max(subset(like, x >= minRating, select = cluster))))
  }
  return(like)
}

getRecommendedMovies <- function(like, mclust, m_title_df){
  df_aux <- data.frame(cbind(m_title_df$MovieID, 
                             clusterNum = mclust$cluster))
  names(df_aux) = c("MovieID", "Cluster")
  if(like == 0){
    recommend <- m_title_df[sample.int(n = nrow(m_title_df), size = 100), 1]
  } else {
    recommend <- as.vector(t(subset(df_aux, Cluster == like, select = MovieID)))
  }
}

getRecommendations <- function(movie_df, user_df, userID){
  mclust <- clusterMovies(movie_df)
  activeUser <- getUserInfo(user_df, userID)
  activeUser <- setUserMovieCluster(movie_df, mclust, activeUser)
  like <- getAverageClusterRating(mclust, activeUser)
  recomendation <- getRecommendedMovies(like, mclust, movie_df)
  recomendation <- recomendation[-activeUser$MovieID]
  movieTitle <- movie_df[match(recomendation, movie_df$MovieID),2]
  recomendation <- data.frame(recomendation, movieTitle)
  return(recomendation)
}

suggestMovies <- function(movie_df, user_df, user_id, num_movies){
  suggestions <- getRecommendations(movie_df, user_df, user_id)
  suggestions <- suggestions[1:num_movies,]
  writeLines("Tal vez te gustaría ver también las siguientes películas:")
  write.table(suggestions[2], row.names = F, col.names = F)
}

max(df_usr$UserID)
suggestMovies(df_mov, df_usr, max(df_usr$UserID), 10)

# Validación del Modelo --------------------------------------------------------
df_custom <- df_cust %>% select(author, movieID, vote_average)

ratings_matrix <- as(df_custom, "realRatingMatrix")
recommender_model <- Recommender(ratings_matrix, method = "UBCF")
set.seed(15)
e <- evaluationScheme(ratings_matrix, method = "split", train = 0.8, given = -5, goodRating = 4)

recommender <- Recommender(getData(e, "train"), method = "UBCF")
predictions <- predict(recommender, getData(e, "known"), type = "ratings")
accuracy <- calcPredictionAccuracy(predictions, getData(e, "unknown"))
print(accuracy)
