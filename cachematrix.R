#makeCacheMatrix handles caching the inverse of a matrix
#cacheSolve performs inverting the matrix

#get fetches the original matrix
#set sets the original matrix
#getInverse fetches the inverse of the original matrix
#setInverse sets the inverse of the orignal matrix
makeCacheMatrix <- function(originalMatrix = matrix()) {
  invertedMatrix <- NULL
  set <- function(newMatrix) {
    originalMatrix <<- newMatrix
    invertedMatrix <<- NULL
  }
  get <- function() { originalMatrix }
  setInverse <- function(theInverse) { invertedMatrix <<- theInverse }
  getInverse <- function() { invertedMatrix }
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}

#returns inverse of matrix.
#either fetches from cache or calculates and stores in cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
