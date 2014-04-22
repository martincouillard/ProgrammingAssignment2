## This function creates a special "matrix" object that can cache its inverse.

## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the matrix inverse (setsolve)
## 4. get the value of the matrix inverse (getsolve)

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(MatrixInverse) s <<- MatrixInverse
  
  getsolve <- function() s
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


##The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting chached matrix inverse")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
    
}
