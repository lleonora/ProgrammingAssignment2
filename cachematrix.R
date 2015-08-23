## The first function creates an object that can store a matrix and its inverse, and that consists
## from four operations: 
## * set matrix
## * get matrix
## * set inverse matrix
## * get inverse


makeCacheMatrix <- function(matr = matrix()) {
  invmat <- NULL      # initializing the inverse matrix 
  # If it is NULL, the next function (cacheSolve) will 
  # calculate the inverse. 
  # If it is not NULL, the cacheSolve takes this value.
  set <- function(y) {  # the function for resetting the matrix.
    if (!is.matrix(y)) stop('your object is not a matrix! Enter a matrix')
    matr <<- y            # It updates the matrix
    invmat <<- NULL       # and replaces its previous inverse with NULL
  }
  get <- function() matr
  setinverse <- function(inversematr) invmat <<- inversematr  # if I use "<-" here,
  # the invmat will be created and used only inside this function. To change the
  # invmat in the parent function, I should use "<<-"
  getinverse <- function() invmat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function calculates the inverse or takes it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...) # what if I omit "..." here?
  x$setinverse(invmat)
  invmat
}
