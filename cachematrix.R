## Put comments here that give an overall description of what your
## functions do
#test
#x1 <- matrix(c(34, 32, 43, 54, 54, 32, 4, 39, 21), 3, 3)
#my_matrix <- makeCacheMatrix(x1)
#my_matrix$get()
#cacheSolve(my_matrix)



## Write a short comment describing this function

#this function is to caching the matrix 
makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverse <<- inverse
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse) 
}


## Write a short comment describing this function

#this function is to read the caching matrix and inverse it
cacheSolve <- function(x, ...)
{
    inverse <- x$getInverse()
    if (!is.null(inverse)) 
    {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...) #key function to inverse the matrix!
    x$setInverse(inverse)
    inverse
}
