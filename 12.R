### makeCacheMatrix() 

#Function **makeCacheMatrix()** create a list to

        1. set the value of the matrix via function *set()*
        2. get the value of the matrix via function *get()*
        3. set the value of the inverse matrix via function *setInverse()*
        4. get the value of the inverse matrix via function *getInverse*
        

MakeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,getInverse = getInverse)
}


### Function cacheSolve()

Function **cacheSolve()** calculates the inverse of the matrix passed by **makeCacheMatrix()**.

Checks if the inverse has already been calculated.

1. If calculated, 
*  It gets the inverse from cache 
*  Skips the computation.   

2. Otherwise, 
*  It calculates the inverse of  matrix 
*  Sets the value of the inverse in the cache via  setinverse function.



CacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting Cached Data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

