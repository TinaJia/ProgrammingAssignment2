# This function creates a special "matrix" object that can cache its inverse.
# This function creates a list with 4 functions: set, get, setinverse, getinverse.
# a special assignment operator: '<<-' allows variables in the function are accessible globally. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL   # define the inverse of the matrix, which store the results later
        # this subset function sets the objective matrix
        set <- function(y){
                x <<- y
                s <- NULL
        }
        get <-function() x  # this function return the input matrix
        setinverse <- function(inverse) s <<-inverse  # this function sets the inverse matrix 
        getinverse <- function() s     # this function returns the inverse matrix
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
        # returns a list of the 4 functions. So we can use 
        # a <- makeCacheMatrix(matrix_1) to set a matrix
        # a$set(matrix_2) to set a new matrix
        # a$setinverse(matrix_3)  to set the inverse matrix
        # a$get to get the setted matrix
        # a$getinverse to get the setted inverse matrix
}


# This function computes the inverse of 
#    the special "matrix" returned by makeCacheMatrix above. 
#    If the inverse has already been calculated 
#    (and the matrix has not changed), then the cachesolve 
#    should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'        
        s <- x$getinverse() # to get the inverse matrix from matrix 'x'. 
        # if it is cached data, which is already there, 
        # it will just return the inverse matrix
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        # if it is not there, we first get the matrix
        # and then calculate the inverse of the matrix
        # then set the inverse matrix to the object, so we do not need to cacluate next time again
        # finally return the inverse matrix
        data <- x$get()
        s <- solve(data)
        x$setinverse(s)
        s
}


# to test the function
a<-makeCacheMatrix() 
a$set(matrix(rnorm(4),2,2))  # first set a random matrix with 2X2
cacheSolve(a)    # after a matrix is gerenatded, this is calcualte the inverse of the matrix
cacheSolve(a)    # this is retrieve the inverse of the matrix
