## Melvin Lopez - R Programming Assignment - Week 3
## This group of functions calculate the inverse of a matrix and caches the result
##  
##  Matrix Test 1 (rounding not required) : m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##  Matrix Test 2 (rounding required)     : m1 <- matrix(c(4, 2, 7, 6), 2, 2)
##  Invertible Matrix Test 1: (Error)     : m1 <- matrix(c(3,4,6,8), 2, 2)
##  
##  
makeCacheMatrix <- function(x = matrix()){ ## Create function makeCacheMatrix with default empty matrix
  m <- NULL 
      if(!is.null(m)) { ## Checks to see if m is not NULL (i.e. if cache is not empty return 
                        ## the inverse of the matrix)
        message("getting cached data")
        return(m)
      }
  
  set <- function(y) { ## Create set function 
    x <<- y            ## assign input argument y to x in the parent environment
    m <<- NULL         ## Sets cache variable to NULL
  }
## Use lexical scoping to retrieve the imput argument from parent environment for the get function  
  get <- function() x

## Define input argument for setter from m in parent environment
  setinverse <- function(inverse) m <<- inverse

## Define getter for mean of m by retrieving parent environment argument m (outside the parenthesis)
  getinverse <- function() m

## Create a list of functions and return it to the parent environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Create function with one defined argument x and any additional arguments that are not defined "..."
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Attempts to retrieve the from the object x by using the getmean function 
  ## (this function only exists in makevector)
  m <- x$getinverse()

  if(!is.null(m)) { ## Checks to see if m is NULL (i.e. never run before, 
    ## if not NULL return the valid cached mean
    message("getting cached data")
    return(m)
  }
    data <- x$get() ## Takes argument x from cachematrix to calculate the mean using getinverse function
    if (det(data) != 0) { ## Checks to see if the matrix has a determinant (i.e not equal to 0)
      m <- round(solve(data,...)) ## calculate the inverse of the matrix and round the result
      x$setinverse(m) ## Set the inverse of the maxtrix as the input value to m and return the value
      m }
      else {
        message("Error: There is no inverse for this matrix!")  ## If the matrix is invertible, return an error
                                                                ## and exit the function
      
      }
}
  

