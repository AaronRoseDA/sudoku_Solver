rm(list = ls())
library(gtools)

puzzles <- read.csv("C:/Users/amrbm/OneDrive/Desktop/medium.txt")
names(puzzles) <- "puzzles"
sudokus <- list()

process_sudoku_string <- function(sudoku_string) {
  # sudoku_string <- puzzles$puzzles[1]
  puzzle_part <- substr(sudoku_string, 14, 14+80)
  puzzle_part <- gsub("[^0-9]", "0", puzzle_part)
  puzzle_numbers <- as.numeric(strsplit(puzzle_part, "")[[1]])
  sudoku_matrix <- matrix(puzzle_numbers, nrow = 9,ncol = 9, byrow = TRUE)
  rownames(sudoku_matrix) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9')
  colnames(sudoku_matrix) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9')
  return(sudoku_matrix)
}

check <- function(sudoku){
  # sudoku <- list(matrix(rep(1:9, 9), nrow = 9, ncol = 9))
  # rownames(sudoku) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9')
  # colnames(sudoku) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9')
  
  g1 <- all(c(1:9) %in% sudoku[[1]][1:3,1:3])
  g2 <- all(c(1:9) %in% sudoku[[1]][1:3,4:6])
  g3 <- all(c(1:9) %in% sudoku[[1]][1:3,7:9])
  g4 <- all(c(1:9) %in% sudoku[[1]][4:6,1:3])
  g5 <- all(c(1:9) %in% sudoku[[1]][4:6,4:6])
  g6 <- all(c(1:9) %in% sudoku[[1]][4:6,7:9])
  g7 <- all(c(1:9) %in% sudoku[[1]][7:9,1:3])
  g8 <- all(c(1:9) %in% sudoku[[1]][7:9,4:6])
  g9 <- all(c(1:9) %in% sudoku[[1]][7:9,7:9])
  
  #check horizontal rows
  h1 <- all(c(1:9) %in% sudoku[[1]][1,1:9])
  h2 <- all(c(1:9) %in% sudoku[[1]][2,1:9])
  h3 <- all(c(1:9) %in% sudoku[[1]][3,1:9])
  h4 <- all(c(1:9) %in% sudoku[[1]][4,1:9])
  h5 <- all(c(1:9) %in% sudoku[[1]][5,1:9])
  h6 <- all(c(1:9) %in% sudoku[[1]][6,1:9])
  h7 <- all(c(1:9) %in% sudoku[[1]][7,1:9])
  h8 <- all(c(1:9) %in% sudoku[[1]][8,1:9])
  h9 <- all(c(1:9) %in% sudoku[[1]][9,1:9])
  
  #check vertical rows
  v1 <- all(c(1:9) %in% sudoku[[1]][1:9,1])
  v2 <- all(c(1:9) %in% sudoku[[1]][1:9,2])
  v3 <- all(c(1:9) %in% sudoku[[1]][1:9,3])
  v4 <- all(c(1:9) %in% sudoku[[1]][1:9,4])
  v5 <- all(c(1:9) %in% sudoku[[1]][1:9,5])
  v6 <- all(c(1:9) %in% sudoku[[1]][1:9,6])
  v7 <- all(c(1:9) %in% sudoku[[1]][1:9,7])
  v8 <- all(c(1:9) %in% sudoku[[1]][1:9,8])
  v9 <- all(c(1:9) %in% sudoku[[1]][1:9,9])
  
  g <- all(g1, g2, g3, g4, g5, g6, g7, g8, g9)
  h <- all(h1, h2, h3, h4, h5, h6, h7, h8, h9)
  v <- all(v1, v2, v3, v4, v5, v6, v7, v8, v9)
  
  all <- all(g,h,v)
 
}

sudoku_group <- function(index) {
  # Ensure the index is within the valid range
  if (index < 1 || index > 81) {
    stop("Index must be between 1 and 81")
  }
  
  # Calculate the row and column for the index
  row_index <- (index - 1) %/% 9 + 1
  col_index <- (index - 1) %% 9 + 1
  
  # Determine the 3x3 group for row and column
  row_group <- ((row_index - 1) %/% 3) * 3 + 1:3
  col_group <- ((col_index - 1) %/% 3) * 3 + 1:3
  
  # Return the row and column groups
  return(list(row_group = row_group, col_group = col_group))
}

get_range <- function(n){
  ((n - 1) %/% 3) * 3 + 1:3
}

group_coord <- function(i, ii){
  # get_range <- function(n) ((n - 1) %/% 3) * 3 + 1:3
  return(list(x = get_range(ii), y = get_range(i)))
}

valid_numbers <- function(puzzle_num){
  puzzle_num <- 1
  sudoku <- process_sudoku_string(puzzles$puzzles[puzzle_num])
  
  valid_numbers <- data.frame(index = 1:81)
  # cell_value <- 1
  # i <- 1
  # ii <- 1
  for (cell_value in 1:9) {
    tf_matrix <- sudoku[1:9, 1:9] != cell_value
    temp <- c()
    #print(tf_matrix)
    for (i in 1:9) {
      for (ii in 1:9) {
        xy <- group_coord(i, ii)
        cell_value %in% sudoku[xy$y, xy$x]
        col <- all(tf_matrix[i, 1:9], tf_matrix[1:9, ii], cell_value)
        #print(col)
        temp[9 * (i - 1) + ii] <- col
      }
    }
    valid_numbers[cell_value+ 1] <- temp
    #print(temp)
  }
  names(valid_numbers) <- c('Index', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  # valid_numbers[,-1] <- lapply(valid_numbers[,-1], function(x) !x)
  valid_numbers$isGiven <- c(t(sudoku)) != 0
  return(valid_numbers)
}

# Define the function
is_valid_sudoku <- function(numbers) {
  # Check if the vector length is within the valid range
  if (length(numbers) < 1 || length(numbers) > 81) {
    return(FALSE)
  }
  
  # Check if all numbers are between 0 and 9 (0 represents an unsolved cell)
  if (any(numbers < 0, numbers > 9)) {
    return(FALSE)
  }
  
  # Function to check duplicates in a group
  has_duplicates <- function(group) {
    group <- group[group != 0] # Ignore zeros (unsolved cells)
    length(unique(group)) != length(group)
  }
  
  # Create an empty 9x9 Sudoku board
  board <- matrix(0, nrow = 9, ncol = 9)
  
  # Fill the board with the numbers
  for (i in 1:length(numbers)) {
    row_index <- (i - 1) %/% 9 + 1
    col_index <- (i - 1) %% 9 + 1
    board[row_index, col_index] <- numbers[i]
  }
  
  # Check rows, columns, and 3x3 grids
  for (i in 1:9) {
    if (has_duplicates(board[i, ]) || has_duplicates(board[, i])) {
      return(FALSE)
    }
  }
  for (row_start in seq(1, 9, by = 3)) {
    for (col_start in seq(1, 9, by = 3)) {
      if (has_duplicates(as.vector(board[row_start:(row_start + 2), col_start:(col_start + 2)]))) {
        return(FALSE)
      }
    }
  }
  
  # Return TRUE if all checks pass
  return(TRUE)
}

is_valid_sudoku(c(1,2))




sudoku <- process_sudoku_string(puzzles$puzzles[1])
valid_nums <- valid_numbers(1)
valid_nums$free_value <- apply(valid_nums[, 2:10], 1, function(x) ifelse(sum(as.numeric(x) == 1) == 1, 1, 0))

valid_nums[, 2:10], 2,function(x) ifelse(sum(as.numeric(valid_nums[1, 2:10]) == 1) == 1, 1, 0))


open_spots <- which(!valid_nums$isGiven)
run <- 0


rm(list = ls())

isValidSudoku <- function(grid) {
  # Ensure the grid is of length 16
  if(length(grid) > 16) {
    return(FALSE)
  }
  
  # Convert the vector to a 4x4 matrix
  matrix_grid <- matrix(grid, nrow = 4, byrow = TRUE)
  
  # Function to check rows, columns, and subgrids
  check_set <- function(set) {
    # Filter out zeros
    non_zero_set <- set[set != 0]
    
    # Check if there are any duplicates in the non-zero elements
    return(all(table(non_zero_set) <= 1))
  }
  
  # Check rows and columns
  for (i in 1:4) {
    if (!check_set(matrix_grid[i, ]) || !check_set(matrix_grid[, i])) {
      # cat("Failed at row or column", i, "\n")
      return(FALSE)
    }
  }
  
  # Check 2x2 subgrids
  for (row in seq(1, 4, by = 2)) {
    for (col in seq(1, 4, by = 2)) {
      subgrid <- matrix_grid[row:(row+1), col:(col+1)]
      if (!check_set(as.vector(subgrid))) {
        # cat("Failed at subgrid starting at", row, col, "\n")
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}

sudoku <- c(1,0,4,0,
            0,0,0,0,
            2,0,1,0,
            0,1,0,4)

valid_nums <- list(c(1),
                   c(2,3),
                   c(4),
                   c(2,3),
                   c(3,4),
                   c(2,3,4),
                   c(2,3),
                   c(1,2,3),
                   c(2),
                   c(2,4),
                   c(1),
                   c(3),
                   c(3),
                   c(1),
                   c(2,3),
                   c(4))

valid_nums_length <- c(lapply(valid_nums, function(x){length(x)}))

valid_nums_index <- c(rep(1,16))
sudoku_index <- 1
sudoku_try <- sudoku
while (sudoku_index != 16) {
  # print('outer')
  sudoku_try[sudoku_index] <- unlist(valid_nums[sudoku_index][[1]][valid_nums_index[sudoku_index]])
  sudoku_try[(sudoku_index + 1):16] <- sudoku[(sudoku_index + 1):16]
  # print(sudoku_try)
  print(valid_nums_index[1:sudoku_index])
  if (isValidSudoku(sudoku_try) == FALSE) {
    sudoku_try_temp <- sudoku_index
    valid_increment <- FALSE
    while (valid_increment == FALSE) {
      if (valid_nums_index[sudoku_try_temp] + 1 > valid_nums_length[sudoku_try_temp] & sudoku_try_temp > 1) {
        sudoku_try_temp <- sudoku_try_temp - 1
      }
      if (valid_nums_length[sudoku_try_temp] == 1 & sudoku_try_temp > 1) {
        sudoku_try_temp <- sudoku_try_temp - 1
      }
      if (valid_nums_index[sudoku_try_temp] + 1 <= valid_nums_length[sudoku_try_temp] &
          valid_nums_length[sudoku_try_temp] != 1) {
        
        valid_nums_index[sudoku_try_temp] <- valid_nums_index[sudoku_try_temp] + 1
        valid_increment <- TRUE
      }
      if ((valid_nums_length[sudoku_try_temp] == 1 | sudoku_try_temp == 1) & valid_increment == FALSE) {
        sudoku_try_temp <- sudoku_try_temp + 1
        valid_increment <- TRUE
        # stop()
      }
      valid_nums_index[(sudoku_try_temp + 1):16] <- 1
      sudoku_index <- sudoku_try_temp
      # print('inner')
    }
    # valid_nums_index[(sudoku_try_temp + 1):16] <- 1
    # sudoku_index <- sudoku_try_temp
    #sudoku_try <- sudoku
  }else{
  sudoku_index <- sudoku_index + 1
  }
}





























