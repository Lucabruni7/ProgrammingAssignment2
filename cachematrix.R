## Put comments here that give an overall description of what your
## functions do

## Firtsly it control the invertibility of the matrix, if there are no error the matrix is chaced.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        
        set <- function (y) {
                if (!is.matrix(y)) stop("Error: it is not a matrix")
                if (nrow(y) != ncol(y)) stop("Error: squared matrix")
                if (det(y) == 0) stop("error: non-invertable matrix (det=0)")
                
                else  {     
                        x_inv <<- NULL
                        x <<- y
                } 
        }
        
        get <- function() x
        setinv <- function(inv) x_inv <<- inv
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## firstly it control that the inv isn't stored in the chace,if it isn't, it 
## calculate the inv and it store it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv
}
