## The following is a pair of functions that can cache the inverse of a matrix

## This function creates a special "matrix", which actually is a list 
## containning the function to: 1, set the value of the matrix   2, get the value of matrix,
## 3, set the value of the inverse of original matrix     4, get the value of the inverse of original matrix

makeCacheMatrix <- function(x = matrix()) {      
         s<-NULL
	 set<-function(y)   {
	 	    x<<-y
	 	    s<<-NULL
	 }
	 get<-function() x
	 setsolve<-function(solve) s<<-solve
	 getsolve<-function() s
	 list(set=set, get=get,
	      setsolve=setsolve, 
	      getsolve=getsolve)

}


## This function calculates the inverse of the above matrix. But if the inverse of the matrix already exists,
## it will get the inverse directly from cache, otherwise, it will calculate the inverse by the function setsolve.

cacheSolve <- function(x, ...) {
       s<-x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
     }
     data<-x$get()
     s<-solve(data, ...)
     x$setsolve(s)
     s
}
