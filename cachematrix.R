## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates a matrix object that stores the original data as well as caches its inverse; it also contains accessor functions to get 
#and set the original and inverse matrix 
makeCacheMatrix <- function(x = matrix()) {     #the matrix to be solved is the input to function 
        I <- NULL                               # store the inverse in I
	get <- function() x                     # function to return the matrix stored in this object
	set <- function(y) {                    # function to update the matrix object stored and set it to the input matrix in this function 
                x <<- y
                I <<- NULL		#reset Inverse because original matrix has changed, the cached data is now stale
        }
	set_inv <- function(inverse_matrix) I <<- inverse_matrix        #function to set inverse of a matrix to the input of this function 
	get_inv <- function()  I                                         #returns the inverse 
	list(set = set, get = get, get_inv = get_inv, set_inv = set_inv)        #return a list with all the 'get' and 'set' functions as its elements
}


## Write a short comment describing this function
# this function computes the inverse of the matrix object returned by makeCacheMatrix. In case the inverse is already computed, the cached result is returned 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I1 <- x$get_inv()                       # If the inverse was already calculated and cached, it is retrieved
                                                # If inverse is not calculated before I1 would be NULL as initialized in makeCacheMatrix function
        if(!is.null(I1)) {                      # In case the inverse is cached along with the matrix, notify that cached data is being returned and return the stored inverse matrix
                message("getting cached data")
                return(I1)
        }
        data <- x$get()                         #get the original matrix stored in object 'x'
        I1 <- solve(data)                       #solve for the inverse of matrix stored in data
        x$set_inv(I1)                           #set the inverse object in 'x' for future use 
        I1                                      #return inverse calculated for current call of cacheSolve()
}

#example use of above code:
#MatrixObj <- makeCacheMatrix(m) ----------- m is an invertible matrix
#MatrixObj$get_inv()
# > NULL  ------------------- right now the inverse object is NULL
#cacheSolve(MatrixObj)
#MatrixObj$get_inv()
# > Inverse of the matrix is returned
