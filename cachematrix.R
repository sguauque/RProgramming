
#first function: "makeCacheMatrix" creates the list containing a function to

#set the matrix
#get the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Second function: "cacheSolve" calculates the inverse of the matrix created with the above function. 
#It first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

#Run the funtion:
#mx = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
#x <- makeCacheMatrix(mx)
#> x$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> cacheSolve(x)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> cacheSolve(x)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#An error message appears if the matrix is singular and cannot be inverted.
