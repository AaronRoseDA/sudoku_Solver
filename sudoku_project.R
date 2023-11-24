rm(list = ls())
start_time <- Sys.time()
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

group_coord <- function(i, ii){
  get_range <- function(n) ((n - 1) %/% 3) * 3 + 1:3
  return(list(x = get_range(ii), y = get_range(i)))
}

valid_Numbers <- function(puzzle_num){
  # puzzle_num <- 1
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
  # Check if the vector length is within the valid range and the numbers are between 0 and 9
  if (length(numbers) != 81 || any(numbers < 0 | numbers > 9)) {
    return(FALSE)
  }
  
  # Convert the numbers into a matrix
  board <- matrix(numbers, nrow = 9, byrow = TRUE)
  
  # Function to check duplicates in a group
  has_duplicates <- function(group) {
    group_non_zero <- group[group != 0]
    length(unique(group_non_zero)) != length(group_non_zero)
  }
  
  # Check rows and columns
  if (any(apply(board, 1, has_duplicates)) || any(apply(board, 2, has_duplicates))) {
    return(FALSE)
  }
  
  # Check 3x3 subgrids
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

getValidNumbers <- function(row) {
  # row <- valid_nums[1,]
  # Find indices where the value is TRUE
  if (row[11]) {
    return(row[12])
  }
  valid_indices <- as.numeric(which(row[2:10] == TRUE))
  return(valid_indices)
}

sudoku_number <- 4
sudoku <- c(t(process_sudoku_string(puzzles$puzzles[sudoku_number])))
valid_numbers <- valid_Numbers(sudoku_number)
valid_numbers$givenNum <- sudoku
valid_nums <- apply(valid_numbers, 1, getValidNumbers)
valid_nums_length <- c(lapply(valid_nums, function(x){length(x)}))

valid_nums_index <- c(rep(1,81))
sudoku_index <- 1
sudoku_try <- sudoku
max_sudoku_index <-0
trys <- 0
is_valid <- FALSE

while (0 %in% sudoku_try | is_valid == FALSE) {
  trys <- trys + 1
  sudoku_try[sudoku_index] <- unlist(valid_nums[sudoku_index][[1]][valid_nums_index[sudoku_index]])
  if (sudoku_index != 81) {
    sudoku_try[(sudoku_index + 1):81] <- sudoku[(sudoku_index + 1):81]
  }
  is_valid <- is_valid_sudoku(sudoku_try)
  if (is_valid == FALSE) {
    sudoku_try_temp <- sudoku_index
    valid_increment <- FALSE
    while (valid_increment == FALSE) {
      sub <- FALSE
      if (valid_nums_index[sudoku_try_temp] + 1 > valid_nums_length[sudoku_try_temp] & sudoku_try_temp > 1) {
        sudoku_try_temp <- sudoku_try_temp - 1
        sub <- TRUE
      }
      if (valid_nums_length[sudoku_try_temp] == 1 & sudoku_try_temp > 1) {
        sudoku_try_temp <- sudoku_try_temp - 1
        sub <- TRUE
      }
      if (valid_nums_index[sudoku_try_temp] + 1 <= valid_nums_length[sudoku_try_temp] &
          valid_nums_length[sudoku_try_temp] != 1 & sub == FALSE) {
        
        valid_nums_index[sudoku_try_temp] <- valid_nums_index[sudoku_try_temp] + 1
        valid_increment <- TRUE
      }
      if ((valid_nums_length[sudoku_try_temp] == 1 | sudoku_try_temp == 1) & valid_increment == FALSE  & sub == FALSE) {
        sudoku_try_temp <- sudoku_try_temp + 1
        valid_increment <- TRUE
      }
      if (sudoku_try_temp != 81) {
        valid_nums_index[(sudoku_try_temp + 1):81] <- 1
      }
      sudoku_index <- sudoku_try_temp
    }
  }else{
    if (sudoku_index != 81) {
      sudoku_index <- sudoku_index + 1
    }
  }
  if (sudoku_index >= max_sudoku_index) {
    # print(sudoku_try[1:sudoku_index])
    max_sudoku_index <- sudoku_index
  }
}

print(t(matrix(sudoku_try, nrow = 9, ncol = 9)))
print(Sys.time() - start_time)

















