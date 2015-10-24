##The following two functions will show how the lexical scoping works in R.
#In order to avoid computing the inverse of a matrix one time and another,
#which consumes much time, I have created to functions that create a matrix and
#cache its inverse.
#It is possible thanks to the <<- operator, which lets us assign the inverse of
#the matrix in an environment that is different from the current environment,
#where the functions are created. 

##Let's see how it works. The following function creates a matrix, calculates its
#inverse and stores the result in the cache (global environment in this case).

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##And this second function calculates the inverse of the matrix created in the
#previous function and set the inverse in the cache, only if the inverse has not
#been computed before. If so, it skips the computation and gets the inverse from
#the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}