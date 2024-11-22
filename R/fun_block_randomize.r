#' Generate Random Assignments for Block-Randomized Design
#'
#' Generates a vector of randomized assignments for a block-randomized design with a specified number of treatment arms. The function handles cases where the block size is not a multiple of the number of arms by including incomplete repetitions within the block.
#'
#' @param n Numeric. The total number of subjects or units to assign.
#' @param block_size Numeric. The size of each block.
#' @param arms Numeric. The number of treatment arms (default is 6).
#'
#' @return A numeric vector of length \code{n}, containing the randomized assignments to treatment arms.
#'
#' @details
#' The function generates random assignments within blocks of size \code{block_size}. Each block contains complete repetitions of the treatment arms, and if \code{block_size} is not a multiple of \code{arms}, it includes an incomplete repetition with randomly selected arms. The assignments within each block are randomly shuffled.
#'
#' If the total number of subjects \code{n} is not an exact multiple of \code{block_size}, the function generates enough blocks to cover all subjects and trims the result to length \code{n}.
#'
#' @note
#' A warning is issued if \code{block_size} is not a multiple of \code{arms}.
#'
#' @examples
#' # Example usage:
#' assignments <- fun_block_randomize(n = 100, block_size = 20, arms = 6)
#' table(assignments)
#'
#' # With block size not a multiple of arms
#' assignments <- fun_block_randomize(n = 50, block_size = 17, arms = 5)
#' table(assignments)
#'
#' @export
fun_block_randomize <- function(n, block_size, arms = 6) {
  # This function returns the random table of assignments
  # for a block-randomized design with a given number of arms.

  # If block size / arms not integer, give warning
  if (block_size %% arms != 0) {
    warning("Block size is not a multiple of the number of arms.")
  }

  # If arguments not numeric, give error
  if (!all(sapply(c(n, block_size, arms), is.numeric))) {
    stop("Arguments must be numeric.")
  }

  # Number of (complete) repetitions needed for one block
  n_comp <- {block_size / arms} %>% floor()

  # Number of blocks needed
  n_rep <- {n / block_size} %>% ceiling()

  # Create a vector of assignments
  assign_vec <- rep(0, (n_rep + 1) * arms)

  # Randomly assign the sequences to the vector,
  for (i in 1:(n_rep+1)) {

    act_block <-
      {block_size - n_comp * arms} %>%  # Number of subs for incompl.rep.
      sample(1:arms,.) %>%              # Randomly select incomp.rep.
      c( rep(1:arms,n_comp),.) %>%         # Add complete reps.
      sample(.)                         # Shuffle the order

    assign_vec[((i - 1) * block_size + 1) : (i * block_size)] <- act_block
  }
  assign_vec <- assign_vec[1:n]

  # Return the vector
  return(assign_vec)
}
