## This function creates a matrix object that can also cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ##Set the values of the matrix
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                ##Get the values of the matrix
                get <- function() x
                ##Now set the inverse of the matrix
                setinverse <- function(inverse) m <<- inverse
                #Get the inverse of the matrix
                getinverse <- function() m
                ## Create list containing a function to: set, get, set, get.
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }        


## This function checks the inverse of the matrix returned by the makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed)..
## .. then the cachesolve will just retrieve the inverse from the cache
## If not, it will compute the inverse, and then set the value in the cache via setinverse()

cacheSolve <- function(x = matrix(), ...) {
        ## checks if the inverse has already been calculated
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                ##gets the inversed matrix from the cache, skips the computation
                return(m)
        }
        #Otherwise, computes the inversed matrix from the data
        matrix <- x$get()
        m <- solve(matrix, ...)
        ##Sets the value of the matrix in the cache via the setmatrix function
        x$setinverse(m)
        m
}

## This is a sample run to verify that the functions work:
## > x = rbind(c(3, -1/16), c(-1/16, 3))
## > m = makeCacheMatrix(x)
## > m$get()
##   [,1]    [,2]
## [1,]  3.0000 -0.0625
## [2,] -0.0625  3.0000
## so far so good..

## Now cache:
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 0.33347807 0.00694746
## [2,] 0.00694746 0.33347807
## 

## Let's retrieve the cache
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 0.33347807 0.00694746
## [2,] 0.00694746 0.33347807
## Ok so it got the cached data

## Let's return the matrix inverse now:
## > m$getinverse() 
## [,1]       [,2]
## [1,] 0.33347807 0.00694746
## [2,] 0.00694746 0.33347807
## This return verifies the aboe inverse results