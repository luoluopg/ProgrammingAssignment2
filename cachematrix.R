##this function creats a object that can cache its inverse of a matrix
## and skip the repeated computation
  
  makeCacheMatrix <- function(x = matrix()) {
        ## set a matrix              
        inv <- NULL  
        set <- function (y){
             x <<- y
                 inv <<- NULL
                }
        get <- function () x     ## get the matrix
        setinv <- function (solve) inv <<- inverse  ##set the inverse
        getinv <- function () inv   ## get the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)  ## this list is the input of cacheSolve
  }
  
  

 ## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
 ## If the inverse has already been calculated 
 ##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
  
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
         inv <- X$getinv()
        ## if the inverse has already been calculated, then get the inverse from the cache and skip the computation
         if (!is.null(inv)){
         message("getting cached data")
         return(inv)
         }
        ## if not, then calculate the inverse
         data <- x$get()      
         inv <- Solve(data, ...)
         ## use the setinv function to set the inverse
        x$setinv(inv)
        inv    ##return the inserse of matrix
  }
