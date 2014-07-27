##These functions are designed to generate and store
##an inverse for a matrix.  Once the inverse is created
##it will be stored to the variable I, which can be
##accessed easily from cached memory.

##the makeCacheMatrix function has one primary purpose.
##it generates a LIST of four other functions.  
##These functions allow the cacheSolve function
##to get the original matrix, set the inverse
##of that matrix and retrieve the inverse.
##Once the inverse is created and stored
##futher calls with the cacheSolve function
##will simply retrieve the value from stored memory.
##If the user wants to change the matrix, the 
##SET function must be called with a new 
##matrix provided.  This resets the inverse to NULL.

##also, the <<- operator has a special purpose.  
##This defines the variable values for the current 
##environment AND the parent environment.
##So by using that operator, I (and/or x) will be set
##to a value that can be accessed by ANY of the functions
##in the list that makeCacheMatrix generates.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {		##this function is not called
                x <<- y			##by the cacheSolve function.
                I <<- NULL		##it would be used if the user
        }					##wanted to change the matrix.
        get <- function() {
		x				##retrieves the matrix value provided.
	  }	
        setinverse <- function(inverse) {
		I <<- inverse		##sets I to be the inverse value.
	  }
        getinverse <- function() {
		I				##retrieves the value for I
	  }
        list(set = set, get = get,	##this generates the list of functions.
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve is designed to use the list of functions from 
##makeCacheMatrix.  It does not use the set function.  
##That one is only used if the user commands it due to 
##a decision to change the matrix value.  
##cacheSolve will get the matrix value,
##set the value for the inverse and
##get the value for the inverse.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()	##this checks to see if the 
        if(!is.null(I)) {	##value for the inverse exists.	
                message("getting cached data")	##if so, it does this part
                return(I)		##if not, it moves on to the next part
        }
        data <- x$get()		##if the inverese is not stored, this runs
        I <- solve(data, ...)	##this calls on the value of x with x$get
        x$setinverse(I)		##that is stored to data, which then is put 
        I				##into the function solve to get the inverse
}					##then x$setinverse function is run on I
					##this stores it and displays the inverse.

