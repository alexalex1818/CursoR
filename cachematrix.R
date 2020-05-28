makeCacheMatrix <- function(x = matrix()) {
  g <- NULL
  set <- function(y) {
    x <<- y
    g <<- NULL
  }
  get <- function() x
  setmean <- function(mean) g <<- mean
  getmean <- function() g
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cacheSolve <- function(x, ...) {
  g <- x$getmean()
  if(!is.null(g)) {
    message("getting cached data")
    return(g)
  }
  data <- x$get()
  g <- mean(data, ...)
  x$setmean(g)
  g
}

