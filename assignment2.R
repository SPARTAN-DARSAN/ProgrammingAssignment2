#makeCacheMatrix is a function who consist of set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){x} #to get the matrix 'x'
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function(){inv} #to obtein the inverse of the matrix
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# cacheSolve is a function used to get the cache data

cacheSolve <- function(x, ...){ #gets cache data
  inv <- x$getinverse()
  if(!is.null(inv)){            # check inverse is NULL
    message("getting cached data")
    return(inv)                 #return inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)       #calculate the inverse 
  x$setinverse(inv)
  inv                           # to return a matrix with inverse of 'x'
}
