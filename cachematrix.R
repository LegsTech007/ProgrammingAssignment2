##Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.  May 19, 2015 JM


##makeCacheMatrix creats a special "matrix", which is really a list containing a function to set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve calculates the inverse of the special "matrix" created with makeCacheMAtrix.  It first checks to see if the inverse has laredy been calculated.  If so, it gets the inverse from the cache and skips the computation.  Otherwise it calculates the inverse of the data and sets teh value of the inverse in teh cache via the setmean function (as defined in makeCacheMatrix).

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}