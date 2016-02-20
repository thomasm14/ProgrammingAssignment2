## makeCacheMatrix takes a matrix as input and produces a list of functions that involve the matrix
## cachSolve returns the inverse of the matrix used in makeCacheMatrix and saves its value so that if called again, 
## the inverse does not need to be recalculated.

# The output of makeCacheMatrix is a list containing these functions:
# "set" overrides the previous input matrix and replaces it with a new one
# "get" returns the input matrix
# "setinverse" assigns the inverse of the input matrix to the symbol inv
# "getinverse" retrieves the value of inv (i.e. the inverse of the input matrix)

makeCacheMatrix <- function(x = matrix()) 
      {
            inv <- NULL
            set <- function(y=matrix()) {  
                  x <<- y
                  inv <<- NULL  
            }
            get <- function() x 
            setinverse <- function(inv_matrix) {inv <<- inv_matrix}  
            getinverse <- function() inv
            list(set = set, get = get, 
                 setinverse = setinverse,   
                 getinverse = getinverse)
      }



## cacheSolve takes the output of makeCacheMatrix as an input and returns the value assigned to inv (the inverse of makeCacheMatrix's input matrix)
## It uses setinverse to assign a value to inv so that the next time cachSolve is called, it does not need to re-run the entire calculation
## if the input matrix is not invertible, cachSolve will produce an error
cacheSolve <- function(b, ...) {  
      inv <- b$getinverse()
      if(!is.null(inv)) {   
            message("getting cached data")
            return(inv)
      }                   
      data_matrix <- b$get()  
      inv <- solve(data_matrix) 
      b$setinverse(inv)  
      inv
}
