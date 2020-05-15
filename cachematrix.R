makeCacheMatrix <- function( mat = matrix() ) {
    ## set inverse to null
    cm <- NULL
    
    # setmat and getmat are functions to set and get matrix
    setmat <- function( matrix ) {
            mat <<- matrix
            cm <<- NULL
    }
    getmat <- function() {
    	mat
    }

    # setInv and getInv are functions to set and get inverse of matrix    
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

    #return the inverse if its already set
    if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
    }
    data <- x$getmat()
    ## Calculate the inverse of matrix
    mat <- solve(data) %*% data

    x$setInv(mat)
    ## Return the matrix
    mat
}
