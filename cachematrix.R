## Making functions makeCacheMatrix and cacheSolve that cache the inverse of a matrix


## This function below is used to make a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initializing the null property
    j <- NULL

    ## Below Methods used to set the matrix and get the matrix
    set <- function( matrix ) {
            m <<- matrix
            j <<- NULL
    }
    get <- function() {
    	m
    }
    ## Setting inverse of the matrix
    setInverse <- function(inverse) {
        j <<- inverse
    }
    ## Getting the inverse of the matrix
    getInverse <- function() {
        j
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
}


## This function below is used to compute the inverse of the matrix returned by above function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## To return the inverse if it is already been calculated
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix
    data <- x$get()

    ## Calculating the inverse 
    m <- solve(data) %*% data

    ## Setting the inverse 
    x$setInverse(m)
    m
}
