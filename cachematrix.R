#makeCacheMatrix: This function creates a special "matrix" OBJECT that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.

##Function sets/gets the matrix object and sets/gets its inverse.
makeCacheMatrix <- function(x = matrix()) { 
    #input for this function is an inversible matrix
    inverse <- NULL #initial value of inverse is zero
    
    set <- function(y) { #set a new matrix to the object
      x <<- y 
      inverse <<- NULL
    }
    
    get <- function() x # return the matrix value
    
    setinverse <- function(solve) inverse <<- solve #use the solve function to get the inverse
    
    getinverse <- function() inverse #retrieve inverse
    
    # assign functions that can be called
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function gets the inverse value, if it does not exist it calculates it and stores it back in the object
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse() #retrieve inverse from object x
  
  if(!is.null(inverse)) { #if it exists return the inverse
    #message("getting cached data")
    return(inverse)  #return cached inverse
  }
  
  #else if inverse does not exist
  data <- x$get() #get the matrix value
  
  inverse <- solve(data, ...) #solve the matrix value
  x$setinverse(inverse) #set the calculated inverse value
  
  inverse #return freshly calculated inverse 
}

# TEST
#
# mat <- makeCacheMatrix(matrix(c(4, 3, 3, 2), ncol = 2, nrow = 2))
# cacheSolve(mat) #calculate the inverse
# cacheSolve(mat) #get the inverse from cache
