## Programming Assigment 2 (Week 3) - Lexical Scoping

## Write a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() {i}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a function that computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Define a test matrix to test the function
test <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

# Run function
cacheSolve(test)