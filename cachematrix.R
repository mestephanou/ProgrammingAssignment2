# create function which returns a list of functions
#it aims to store a martix and a cached value of the inverse of the matrix
  make_cache_matrix <- function(x = numeric()) {
  
  #holds the cached value or NULL if nothing is cached
  #initially nothing is cached so set it to NULL
  cache <- NULL
  
  #store a matrix
  set_matrix <- function(new_value) {
    x <<- new_value
    
    #since the matrix is assigned a new value, flush the cache
    cache <<- NULL
}
  
#returns stored matrix
  get_matrix <- function() {
    x
}
  
#cache the given argument 
  cache_inverse <- function(solve) {
    cache <<- solve
}
  
#get the cached value
  get_inverse <- function() {
    cache
}
  
# return a list. Each named element of the list is a function
  list(set_matrix = set_matrix, get_matrix = get_matrix, cache_inverse = cache_inverse, get_inverse = get_inverse)
}


# The following function calculates the inverse 
#of a "special" matrix created with make_cache_matrix
  cache_solve <- function(y, ...) {
  
    #get the cached value
    inverse <- y$get_inverse()
    
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
}
  
  #otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$get_matrix()
  inverse <- solve(data)
  y$cache_inverse(inverse)
  
#return the inverse
  inverse
}
  
# a small example to check that the function works properly  
i <- make_cache_matrix( matrix(c(20,50, 100, 200), nrow = 2, ncol = 2) )
summary (i)
i$get_matrix()
cache_solve(i)

#rerun the cache_solve for a second time to get he cached value
cache_solve(i)