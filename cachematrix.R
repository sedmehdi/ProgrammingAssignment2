## The functions are going to implement inverse-cachable matrixes.

## This function makes a structure around the given matrix
## which can hold the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	get <- function(){
		x
	}
	
	setInv <- function(newInv){
		inv <<- newInv
	}
	
	getInv <- function(){
		inv
	}
	
	list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## The following method supposes that x is an output of the above method.
## At first it checks if x contains the inverse.
## If x does not, then the method calclulates x's inverse and stores in it.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if (!is.null(inv)){
        	message("getting cached data")        	
        	return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data)
        
        x$setInv(inv)
        
        inv        
}
