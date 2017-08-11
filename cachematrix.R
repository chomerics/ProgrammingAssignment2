################################################################################
## Coursera R Programming HW Week 3 Lixicon Scoping
## Create makeCacheMatrix function which will allow a matrix to be cached
## the function uses <<- to store the variable in a parent state for caching


makeCacheMatrix <- function(x = matrix()) {
    MatInv <- NULL
    
    ## set the value of the matrix, reset MatInv
    set <- function(y) {
        x <<- y
        MatInv <<- NULL
    }
    
    ## get the value of the matrix
    get <- function(){
        x
    } 
    
    ## set the matrix inverse
    setMatInv <- function(inverse){
        MatInv <<- inverse
    }
    
    ## get the matrix inverse
    getMatInv <- function(){
        MatInv
    } 
    
    ## list everything after the function runs
    list(set = set, get = get,
         setMatInv = setMatInv,
         getMatInv = getMatInv)
}


## This function first gets the matrix inverse stored in x$gatMatInv
## It then checks to see if there was a value in x$getMatInv
## If there is a value, it uses the cached value instead or running the calc

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    MatInv <- x$getMatInv()
    
    ## checl to see if the matrix has been cached, if it is, use it
    if (!is.null(MatInv)) {
        message("getting cached data")
        return(MatInv)
    }
    
    ## get the matrix from x$get()
    matrx <- x$get()
    
    ## solve for the inverse matrix (1/(ad-bc)[(d, -b),(a, -c)])
    MatInv <- solve(matrx, ...)
    
    ## set the inverted matrix.  This caches the matrix inverse in setMatInv()
    x$setMatInv(MatInv)
    
    ## show the value of the inverted matrix
    MatInv
}
