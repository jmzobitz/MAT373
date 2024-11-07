# MAT 373, Day 30

# Sequencium simulation


# We need to define two helper functions: 
# (1) If a point is a neighbor of current points in the grid (function are_neighbors)
# (2) Conduct a turn that randomly selects a place to play (function sequencium turn)
# (3) Run a whole game of sequencium, returning the final grid (function sequencium_game)


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

sequencium_game <- function(input_grid) {
  
  grid <- input_grid
  in_play <- (grid |> filter(player == "N") |> nrow()) > 0
  
  
  while(in_play) {
    grid <- sequencium_turn("R",grid)
    grid <- sequencium_turn("B",grid)
    in_play <- (grid |> filter(player == "N") |> nrow()) > 0
  }
  
  
  return(grid)
  
  
}

starting_grid <- expand_grid(x_coord=1:4,y_coord=1:4) |>
  mutate(point = 1:n()) |>
  relocate(point) |>
  mutate(player = "N",
         player = if_else(point %in% c(13,10),"R",player),
         player = if_else(point %in% c(4,7),"B",player),
         value = 0,
         value = if_else(point %in% c(4,13),1,value),
         value = if_else(point %in% c(10,7),2,value))

starting_grid |>
  filter(player !="N") |>
  ggplot(aes(x=x_coord,y=y_coord,color=player)) + 
  geom_label(aes(label=value))

## Make a plot of the current configuration
grid |>
  filter(player !="N") |>
  ggplot(aes(x=x_coord,y=y_coord,color=player)) + 
  geom_label(aes(label=value))





sequencium_game <- function(input_grid) {
  
  grid <- input_grid
  in_play <- (grid |> filter(player == "N") |> nrow()) > 0
  
  
  while(in_play) {
    grid <- sequencium_turn("R",grid)
    grid <- sequencium_turn("B",grid)
    in_play <- (grid |> filter(player == "N") |> nrow()) > 0
  }
  
  
  return(grid)
  
  
}

# Now onto the results.  We play a round and then determine the winners, with "T" meaning a tie
results <- tibble(round = 1:10,
                  grid = map(.x=round,.f=~sequencium_game(starting_grid)),
                  winners = map_chr(.x=grid,.f=function(.x) {
                    curr_winner <- .x |> slice_max(value) |> pull(player) |> unique()
                    if(length(curr_winner)>1) {curr_winner = "T"}
                    
                    return(curr_winner)
                    
                    
                  } ),
                  score = map_dbl(.x=grid,.f=~max(.x$value))
)

# What is an advantageous point to play? (Analyze each of the grid points to determine where you landed) - for winner and losers of grid


