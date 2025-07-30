


# ----- Number Line ML -----



# There are four discrete number lines
# The first space is 1
# The last space is 10
# Each number line has an agent that starts in space 1
# At each step, choose an agent to move
# The chosen agent moves as follows
# 1. If the agent is in space 1, it moves to space 2 with probability 1
# 2. Otherwise, the agent moves -1 with prob. 0.5 and 1 with prob. 0.5
# The game ends when any agent reaches space 10
# Track how many times each agent moves
# Final score is the sum of squared counts
# Smallest score is best



#' Create number line
#' 
#' @description
#' Creates a number line with an agent in the first space. The agent
#' is represented as '1' and empty as '0'.
#' 
#' @returns a number line with an agent in the first space.
create_numberline <- function(){
  return(c(1, rep(0, 9)))
}



# Moving point on the line
# Choice is the number of the current line
move_point <- function(choice, lineList){ 
  n_line <- lineList[[choice]]
  
  # if the point is at 1 on the number line, it always moves right
  if (n_line[1] == 1) {
    n_line[1] <- 0
    n_line[2] <- 1
    
  } else {
    direction <- sample(c(-1, 1), 1)
    curr_pos <- which(n_line == 1)
    n_line[curr_pos] <- 0
    n_line[curr_pos + direction] <- 1
  }
  
  lineList[[choice]] <- n_line
  return(lineList)
}

#' Compute final score
#' 
#' @description 
#' The final score for the game is the sum of the squared times that
#' each number line has been selected. The score should be minimized.
#' 
#' @param counts Integer. A vector of times each line was selected.
#' @returns the final score.
compute_score <- function(counts){
  return(sum(counts^2))
}

#' Choose Naive Action
#' 
#' @description 
#' Chooses number line using naive policies for the number line game.
#' 
#' @details 
#' Always chooses the first number line.
#' 
#' @returns the chosen number line.
choose_naive_action <- function(){
  return(1)
}


state_from_world <- function(count, number_line) {
  if (count > 30 ) {
    count <- 30
  }
  
  state <- data.frame(
    count = count,
    pos = which(number_line == 1)
  )
  return(state)
}


create_policy <- function(state_action_space){
  state_action_space$action <- runif(nrow(state_action_space), 0, 1)
  return(state_action_space)
}


choose_action <- function(state, policy){
  ind <- (state$count == policy$count) & (state$pos == policy$pos)
  action <- policy$action[ind]
  return(action)
}


evaluate_policy <- function(policy, iters = 1000){
  line_set <- list(
    create_numberline(),
    create_numberline(),
    create_numberline(),
    create_numberline()
  )

  score_list <- lapply(1:iters, function(i){
    state <- 0
    count <- c(0, 0, 0, 0)
    while (state == 0) {
      value <- lapply(1:4, function(j){
        n_line <- line_set[[j]]
        move_count <- count[j]
        state <- state_from_world(move_count, n_line)
        action <- choose_action(state, policy)
        return(action)
      })
      value <- unlist(value)
      choice <- which.max(value)
      
      line_set <- move_point(choice, line_set)
      count[choice] <- count[choice] + 1
      curr_line <- line_set[[choice]]
      
      if(curr_line[10] == 1) {
        state <- 1
      }
    }
    
    return(compute_score(count))
  })
  
  score_list <- unlist(score_list)
  return(mean(score_list))
}

# Evaluates population
eval_fitness <- function(population){
  pop_fitness <- lapply(population, function(chrom){
    evaluate_policy(chrom, iters)
  })
  pop_fitness <- unlist(pop_fitness)
  return(pop_fitness)
}


# Crossover
crossover <- function(A, B, crossover_rate){
  p <- runif(nrow(A), 0, 1)
  doCross <- (p < crossover_rate)
  gamma <- runif(nrow(A), 0, 1)
  crosses <- gamma * (A$action - B$action) + A$action
  x <- A
  x$action <- ifelse(doCross, crosses, A$action)
  return(x)
}


# Mutation: Pure exploration - randomly changes values of the chromosomes
mutate <- function(policy, mutation_rate = 0.01){
  p <- runif(nrow(policy), 0, 1)
  m <- runif(nrow(policy), 0, 1)
  policy$action <- ifelse(p < mutation_rate, m, policy$action)
  return(policy)
}


# ----- Running the Optimizer -----

# Parameters
state_action_space <- expand.grid(
  count = (0:30),
  pos = (1:10),
  action = NA
)
pop_size <- 10
crossover_rate <- 0.5
mutation_rate <- 0.01
iters <- 80
epochs <- 15
fitness_record <- rep(NA, epochs)
population <- lapply(1:pop_size, function(i){
  create_policy(state_action_space)
})
plot(NA, xlim = c(1, epochs), ylim = c(2000, 15000))


for (i in 1:epochs) {
  # Evaluate fitness
  pop_fitness <- eval_fitness(population)
  fitness_median <- median(pop_fitness)
  # Selection
  population <- population[pop_fitness <= fitness_median]
  # Crossover
  num_offspring <- pop_size-length(population)
  offspring <- lapply(1:num_offspring, function(i){
    parent_inds <- sample(1:length(population), 2, replace = FALSE)
    crossover(population[[ parent_inds[1] ]], population[[ parent_inds[2] ]], crossover_rate)
  })
  # Mutate
  offspring <- lapply(1:length(offspring), function(i){
    mutate(offspring[[i]], mutation_rate)
  })
  # Update 
  population <- c(population, offspring)
  
  fitness_record[i] <- mean(pop_fitness)
  points(fitness_record, type = 'b')
}
