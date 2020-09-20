
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #1st requirement
    x <<- y
    m <<- NULL
  }
  get <- function() x  #2nd requirement
  setinverse <- function(inverse) m <<- inverse  #3rd requirement
  getinverse <- function() m  #4th requirement
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#cachesolve
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { #comparing process
    message("getting cached data")
    return(m) #retrieved from cache
  }
  data <- x$get()
  m <- solve(data, ...) #not_in_cache
  x$setinverse(m)
  m
  
  
}
