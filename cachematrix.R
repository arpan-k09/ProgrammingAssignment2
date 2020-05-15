makeCacheMatrix <- function( m = matrix() ) {
    cm <- NULL
    setmat <- function( matrix ) {
            mat <<- matrix
            cm <<- NULL
    }
    getmat <- function() {
    	mat
    }

    
    setInv <- function(inverse) {
        cm <<- inverse
    }

    
    getInv <- function() {
        
        cm
    }

    list(setmat = setmat, getmat = getmat,
         setInv = setInv,
         getInv = getInv)
}

cacheSolve <- function(x, ...) {

    mat <- x$getInv()

    
    if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
    }
    data <- x$getmat()

    mat <- solve(data) %*% data

    x$setInv(mat)
    mat
}
