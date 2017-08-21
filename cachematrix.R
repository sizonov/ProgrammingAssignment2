## Wollowing function constructs extended "matrix" object that can cache its inverse
## the constructor implements auxiliary methods: 
##    get - retreive the object data value; 
##    set - assign data value; 
##    getCache - retreive stored inversed matrix data; 
##    setInverse - store inversed matrix data
makeCacheMatrix <- function(x = matrix()) {
  cch <- NULL
  set <- function(y) {
    x <<- y
    cch <<- NULL
  }
  get <- function() x
  setInverse <- function(inverso) cch <<- inverso
  getInverse <- function() cch
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function computes and returns a matrix that is the inverse of its argument
cacheSolve <- function(x, ...) {
  cch <- x$getInverse()
  if(!is.null(cch)) {
    message("cached inveted matrix is:")
    return(cch)
  }
  data <- x$get()
  cch <- solve(data, ...)
  x$setInverse(cch)
  message("new inveted matrix is")
  cch        
}

## Usage:
## 1) Create an invertable matrix
## 2) Pass result of step 1) as an argument to the function makeCacheMatrix
## 3) Call cacheSolve function with argument produced on the step 2)
## Since now cacheSolve function and getInverse method both return cached inverted matrix