## This function take matrix object as an argument and tracks the input matrix and inverse matrix
## and stores in list.



makeCacheMatrix <- function(x = matrix()) {

        In <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(Inverse) In <<- Inverse
        getminv <- function() In
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)


}


## This function performs the inverse matrix using solve and alos check if it cashed by invokig the above function.
## if it is cached then it will not perform solve() [matrix inverse]

cacheSolve <- function(x, ...) {


         In <- x$getminv()
        if(!is.null(In)) {
                message("getting cached data")
                return(In)
        }
        data <- x$get()
        In <- solve(data, ...)
        x$setminv(In)
        In

        ## Return a matrix that is the inverse of 'x'
}
