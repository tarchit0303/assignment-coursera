## Put comments here that give an overall description of what your
## functions do

## Thsi function creates a matrix that can cache inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setInv<-function(solve) inv<<-solve
        getInv<-function() inv
        list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv<-x$getInv()
        if(!is.null(inv)){
                message("getting inverse of matrix")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setInv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
