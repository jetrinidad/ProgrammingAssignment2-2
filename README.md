# ProgrammingAssignment2
Programming assignment, Creating a cache function and returning the cached inverse of a matrix
## First create a square matrix of 2 rows
x<- matrix(1:4,nrow = 2,ncol = 2)
## Get its inverse for validation whether our function returns the correct inverse
solve (x)

## The makeCachematrix function creates a special matrix that
   makeCachematrix <- function(x = matrix()) {
                  m <- NULL
        ## Sets the values of the matrix
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

  ## This function first checks if the inverse has already been computed
  ## If the inverse is not computed yet, it then computes the inverse, returns it and saves it to cached data
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## If the inverse was already computed, the function skips the calculation and returns the cached data of the inverse matrix      
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  }

## We then check if our function returns an accurate inverse matrix and whether our cache function works
solve(x)
y<- makeCachematrix(x)
cacheSolve(y)
