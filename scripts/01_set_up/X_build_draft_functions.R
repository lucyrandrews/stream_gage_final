## BUILD DRAFT FUNCTIONS

# This scripts builds draft custom functions used in this analysis. Functions in
# this script are typically still buggy.

# Functions ----

#' This function computes the most efficient weighted set cover using a greedy
#' algorithm. Currently, the recursive call section lines with the parallelized
#' `mclapply` call are throwing errors. Also, this is incredibly slow for large
#' problems like statewide analysis.

weighted_set_cover <- function(named_sets_w_elements,
                               cost_weights,
                               top_n_return,
                               n_cores = 4) {
  
  cat("Begin weighted set cover...\n")
  
  names(cost_weights) <- names(named_sets_w_elements)
  
  multiplier <- 10
  
  # start with the top (multiplier * top_n_return) most significant sets to
  # reduce computational cost
  max_num_set <- multiplier * top_n_return
  
  # if there are fewer sets than the maximum efficiency number, sort the top
  # sets by increasing cost
  if (length(named_sets_w_elements) > max_num_set) {
    
    index <- order(abs(cost_weights), decreasing = FALSE)
    cost_weights <- cost_weights[index][1:max_num_set]
    named_sets_w_elements <- named_sets_w_elements[index][1:max_num_set]
    
  }
  
  s_hat <- 1.0
  
  # get all unique elements
  all_elements <- unique(unlist(named_sets_w_elements))
  
  # initialize a variable tracking how many elements remain to be covered
  remain <- s_hat * length(all_elements)
  
  # initialize final results which will contain a list of set names
  cur_results <- c()
  
  # grab set names
  all_set_names <- names(named_sets_w_elements)
  
  # evaluate current candidates for marginal gain and size
  mc_results <- mclapply(all_set_names,
                         function(cur_name,
                                  cur_res,
                                  named_sets_w_elements,
                                  cost_weights) {
                           
                           cur_gain <- get_marginal_gain(cur_name,
                                                         cur_res, 
                                                         named_sets_w_elements,
                                                         cost_weights)
                           
                           cur_size <- length(named_sets_w_elements[[cur_name]])
                           
                           return(tibble(set_id = cur_name,
                                         gain = cur_gain,
                                         size = cur_size))
                           
                         },
                         
                         cur_res = cur_results,
                         named_sets_w_elements = named_sets_w_elements,
                         cost_weights = cost_weights,
                         mc.cores = n_cores)
  
  # reshape results of parallel computation
  candidates <- mc_results %>% bind_rows()
  
  # identify how many sets to return - the minimum of the specified return or
  # the count of candidate sets
  top_n_return <- min(top_n_return, nrow(candidates))
  
  for (i in seq(top_n_return)) {
    
    # if there are no more candidate sets, return
    if (nrow(candidates) == 0) {
      
      covered_elements <- unique(unlist(named_sets_w_elements[cur_results]))
      
      s_hat <- length(covered_elements) / length(all_elements)
      
      cat("No more candidate sets, ending weighted set cover\n")
      return(list(top_sets = cur_results, coverage = s_hat))
      
    }
    
    # find the set with maximum marginal gain; if there are ties (two sets have
    # the same marginal gain), pick the one that covers more elements
    candidates <- candidates[order(-candidates$gain, -candidates$size), ]
    
    # update remain to count the remaining uncovered elements
    remain <- remain - length(get_marginal_benefit(candidates[1, "set_id"],
                                                   cur_results,
                                                   named_sets_w_elements))
    
    # add the current top-candidate set to current results
    cur_results <- c(cur_results, candidates[1, "set_id"])
    
    # if all elements are covered, return the solution
    if (remain == 0) {
      
      # list the covered elements
      covered_elements <- unique(unlist(named_sets_w_elements[cur_results]))
      
      # update the coverage
      s_hat <- length(covered_elements) / length(all_elements)
      
      cat("No elements remain; ending weighted set cover\n")
      
      # return the full-coverage solution
      return(list(top_sets = cur_results, coverage = s_hat))
      
    }
    
    # update candidate sets
    # first, remove the candidate set that just been selected
    candidates <- candidates[-1, ]
    
    # recalculate gain, removing rows with gain == 0
    if (nrow(candidates) > 0) {
      
      mc_results <- mclapply(seq(nrow(candidates)),
                             function(row,
                                      candidates,
                                      cur_res,
                                      named_sets_w_elements,
                                      cost_weights) {
                               
                               cur_name <- candidates[row, "set_id"]
                               
                               cur_gain <- get_marginal_gain(cur_name,
                                                             cur_res,
                                                             named_sets_w_elements,
                                                             cost_weights)
                               
                               if(cur_gain != 0) {
                                 
                                 candidates[candidates$set_id == cur_name, "gain"] <- cur_gain
                                 
                                 top_candidate <- candidates[candidates$set_id == cur_name, ]
                                 
                                 return(top_candidate)
                                 
                               }
                               
                             },
                             
                             candidates = candidates,
                             cur_res = cur_results,
                             named_sets_w_elements = named_sets_w_elements,
                             cost_weights = cost_weights,
                             mc.cores = n_cores)
      
      new_candidates <- mc_results %>% bind_rows()
      
      candidates <- new_candidates
      
    }
    
  }
  
  # not fully covered, compute the current coverage and return
  covered_elements <- unique(unlist(named_sets_w_elements[cur_results]))
  
  s_hat <- length(covered_elements) / length(all_elements)
  
  cat("End weighted set cover...\n")
  
  return(list(top_sets = cur_results, coverage = s_hat))
  
}


# return a list of elements from all_elements that have not been covered so far
# cur_set_name: name of the current set
# cur_results: vector of set IDs in current result
get_marginal_benefit <- function(cur_set_name, cur_results, named_sets_w_elements) {
  
  all_elements <- unique(unlist(named_sets_w_elements))
  
  cur_elements <- named_sets_w_elements[[cur_set_name]]
  
  if(length(cur_results) == 0) {
    
    not_covered_elements <- cur_elements
    
  } else {
    
    covered_elements <- unique(unlist(named_sets_w_elements[cur_results]))
    
    not_covered_elements <- setdiff(cur_elements, covered_elements)
    
  }
  
  return(not_covered_elements)
  
}


# evaluate the marginal gain from selecting a set, relative to other sets
get_marginal_gain <- function(cur_set_name,
                              cur_results,
                              named_sets_w_elements,
                              cost_weights) {
  
  abs_cur_cost <- abs(cost_weights[cur_set_name])
  
  cur_marg_benefit <- get_marginal_benefit(cur_set_name,
                                           cur_results,
                                           named_sets_w_elements)
  
  return(length(cur_marg_benefit) / abs_cur_cost)
  
}