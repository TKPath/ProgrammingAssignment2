
## Takes matrix as argument

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## define function to set matrix 
        set2 <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## define function to return input matrix
        get2 <- function() x
        ## define function to invert input matrix with solve() as variable i passed outwith environment
        set_inverse <- function(solve) i <<- solve
        ## define function to simply return inverted input matrix
        get_inverse <- function() i
        ## place the 4 defined functions as self-named list elements
        list(set2=set2, get2=get2, set_inverse=set_inverse, get_inverse=get_inverse)
}


## cacheSolve() takes output from makeCacheMatrix as argument 

cacheSolve <- function(x, ...) {
        ## attempt to get inverted cached matrix
        i <- x$get_inverse()
        ## if cached matrix identified it is retrieved and returned
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        ## otherwise inputted matrix retrieved
        mat <- x$get2()
        ## and inverted
        i <- solve(mat)
        ## set
        x$set_inverse(i)
        ## and returned
        i
}
