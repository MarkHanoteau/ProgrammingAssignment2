
##Set the value of the vector
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##Get the value of the vector
        get <- function() x
        ##Set the value of the mean
        setmean <- function(mean) m <<- mean
        #Get the value of the mean
        getmean <- function() m
        ## this function makeVector, creates a special vector, which is really a list
        ## containing a function to: set, get, set, get.
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## This function calculates the mean of the special vector created above.
cachemean <- function(x, ...) {
        m <- x$getmean()
        ## checks if the mean has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
                ##gets the mean from the cache, skips the computation
                return(m)
        }
        #Otherwise, calculates the mean of the data
        data <- x$get()
        m <- mean(data, ...)
        ##Sets the value of the mean in the cache via the setmean function
        x$setmean(m)
        m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}