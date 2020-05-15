makeCacheMatrix <- function( m = matrix() ) {

	
    cm <- NULL

    set <- function( matrix ) {
            mat <<- matrix
            cm <<- NULL
    }

    get <- function() {
    	mat
    }

    
    setInv <- function(inverse) {
        cm <<- inverse
    }

    
    getInv <- function() {
        
        cm
    }

    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

cacheSolve <- function(x, ...) {

    m <- x$getInv()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInv(m)

    ## Return the matrix
    m
}
