# ProgrammingAssignment2
Programming assignment, Creating a cache function and returning the cached inverse of a matrix
### First create a square matrix of 2 rows and cols
x<- matrix(1:4,nrow = 2,ncol = 2)
### Get its inverse for validation whether our function returns the correct inverse
solve (x)

## The makeCachematrix function creates a special matrix
   
   makeCachematrix <- function(x = matrix()) {
                  m <- NULL

### Sets the values of the matrix
               set <- function(y) {
                x <<- y
                m <<- NULL
            }
        ## Gets the values of the matrix
              get <- function() x
        ## Sets the values of the matrix inverse
              setinverse <- function(solve) m <<- solve
        ## Gets the values of the matrix inverse
              getinverse <- function() m
              list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns the inverse of the special matrix created by the makeCachematrix function
      cacheSolve <- function(x, ...) {


### Check if inverse has already been computed, if yes then skip and return cached inverse
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
### If  inverse not yet calculated, inverse is calculated then saved into cache     
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  }

### Check if caching works and values are accurate
   solve(x)
   y<- makeCachematrix(x)
   cacheSolve(y)
   cacheSolve(y) == solve(x)
