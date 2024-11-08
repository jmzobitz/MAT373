# MAT 373, Day 30

# Sequencium simulation

# First we define some helper functions

# Make a starting grid of play
make_starting_grid <- function(grid_size=4) {
  
  # Define the player names
  players <- c("B","R")
  
  upper_diag <- sample(players,1)
  lower_diag <- players[!(players %in% upper_diag )]
  # Randomly associate each player on the diagonal
  diag_value <-   c(rep(upper_diag,times = grid_size/2),rep(lower_diag,times = grid_size/2))
  
  # Create a blank matrix
  matrix_data <- matrix(data = "N",nrow=grid_size,ncol=grid_size)
  
  # Create the square matrix
  diag(matrix_data) <- diag_value
  
  value_matrix = matrix(data = 0,nrow=grid_size,ncol=grid_size)
  
  diag(value_matrix) <- c(1:(grid_size/2),(grid_size/2):1)
  
  
  # Convert matrix to data frame, set up row and column names
  df <- as.data.frame(matrix_data)
  df$x_coord <- nrow(df):1  # Add an 'x' coordinate
  
  # Pivot to long format
  long_df <- df |>
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "player") |>
    mutate(y_coord = as.integer(gsub("V", "", y_coord)))  # Convert column names to y-coordinates
  
  # Convert matrix to data frame, set up row and column names
  df_value <- as.data.frame(value_matrix)
  df_value$x_coord <- nrow(df_value):1  # Add an 'x' coordinate
  
  # Pivot to long format
  long_df_value <- df_value |>
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "value") |>
    mutate(y_coord = as.integer(gsub("V", "", y_coord)))  # Convert column names to y-coordinates
  
  # Now join the result
  
  starting_grid <- long_df |>
    inner_join(long_df_value,by=c("x_coord","y_coord")) |>
    mutate(point = 1:grid_size^2) |>
    relocate(point)
  
  return(starting_grid)
  
}

# Determine if a function is allowed to be played on
are_neighbors <- function(test_point, list_points) {
  # Given a test point and a list of all points, determine if they are neighbors of each other using an adjacency test.  Include diagonals.
  
  # Extract coordinates
  x1 <- test_point$x_coord
  y1 <- test_point$y_coord
  x2 <- list_points$x_coord
  y2 <- list_points$y_coord
  
  # Check if points are adjacent, including diagonals
  pts <- pmax(abs(x1 - x2), abs(y1 - y2)) == 1
  
  
  return(list_points[pts,])
}

# Start a turn for a player
sequencium_turn <- function(input_player,curr_grid) {
  
  curr_points <- curr_grid |> filter(player == input_player)
  points_available <- curr_grid |> filter(player == "N")
  
  if(nrow(points_available) == 0) {  # Exit if there are no open points to play
    return(curr_grid)
  }
  
  can_move <- curr_points |> 
    mutate(move = map2(.x=x_coord,.y=y_coord,.f=~are_neighbors(tibble(x_coord=.x,y_coord=.y),points_available) ),
           tot_options = map_int(move,nrow))
  
  if (max(can_move$tot_options) == 0) {  # Exit if there are no possible moves
    return(curr_grid)
  }
  # Randomly select a point
  can_play <- can_move |>
    select(value,move) |>
    rename(curr_value = value) |>
    unnest(cols=c(move)) |>
    slice_sample()
  
  new_point <- can_play |>
    mutate(value = curr_value+1,
           player = input_player) |>
    select(-curr_value)
  
  new_grid <- curr_grid |>
    mutate(player = if_else(point == new_point$point,new_point$player,player),
           value = if_else(point == new_point$point,new_point$value,value))
  
  
  return(new_grid)
  
  
  
}

# Play an entire game of sequencium - we have R going first
sequencium_game <- function(input_grid,random_start = TRUE) {
  
  grid <- input_grid
  in_play <- (grid |> filter(player == "N") |> nrow()) > 0
  
  if(random_start) {
  # Randomly determine who goes first
  player_order <- sample(c("B","R"),size=2,replace=FALSE)
  } else {
    player_order<-c("B","R")
  }
  
  while(in_play) {
    grid <- sequencium_turn(player_order[[1]],grid)
    grid <- sequencium_turn(player_order[[2]],grid)
    in_play <- (grid |> filter(player == "N") |> nrow()) > 0
  }
  
  
  return(grid)
  
  
}


# Make the starting grid
starting_grid <- make_starting_grid(4)


# Visualize the starting grid
starting_grid |>
  filter(player !="N") |>
  ggplot(aes(x=x_coord,y=y_coord,color=player)) + 
  geom_label(aes(label=value))


# Sample run of the code are_neighbors
are_neighbors(test_point = tibble(x_coord=4,y_coord=3),
              list_point = tibble(x_coord=c(3,1),y_coord=c(4,2)))


# See a sample turn
sequencium_turn("B",starting_grid)


# Now onto the results.  We play a round and then determine the winners, with "T" meaning a tie
results <- tibble(round = 1:100,
                  grid = map(.x=round,.f=~sequencium_game(starting_grid,random_start = TRUE)),
                  winners = map_chr(.x=grid,.f=function(.x) {
                    curr_winner <- .x |> slice_max(value) |> pull(player) |> unique()
                    if(length(curr_winner)>1) {curr_winner = "T"}
                    
                    return(curr_winner)
                    
                    
                  } ),
                  score = map_dbl(.x=grid,.f=~max(.x$value))
)

# Report out the results:

results |> 
  count(winners) |>
  mutate(prop = n / sum(n))
