## Create a matrix that sets the value of the matrix and then gets the value.  Then set the value of the matrix inverse and get that value
## Functino takes a matrix as its argument and then
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y # assign higher variable
		m <<- NULL # assign higher variable
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m 
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

cacheSolve <- function(x, ...) { #check cache or solve
	m <- x$getinverse() #pull whatever is in x
	if(!is.null(m)){ #check if a matrix is stored
		message('Getting cached matrix inverse')
		return(m)
	}
	data <- x$get() # if no matrix then get the matrix data
	m <- solve(data, ...) #solve matrix
	x$setinverse(m) #store x
	m ## Return a matrix that is the inverse of 'x'
}
