##sample using caching method for mean of a vector
#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
makeVector <- function(x = numeric()) {
        m <- NULL #set m the mean to null for future placeholding
        set <- function(y) { #def a func by lexi scope
                x <<- y #set vector x with vector y that in an environment that is different from the current environment.
                m <<- NULL #vector m reset to null in an environment that is different from current environment
        }
        get <- function() x #get func to return vector x
        setmean <- function(mean) m <<- mean #set function for mean to be placed in m
        getmean <- function() m #get function to return mean
        #return caching of all params defined earlier to working environment
	list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

##sample
#gets the mean from the cache and skips the computation
cachemean <- function(x, ...) {
        m <- x$getmean() #m will store the mean cached in x in previous environment
		
		#m is not null, contains cache mean from previous.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		#m is null, create vector data and store vector used from previous environment 
        data <- x$get()
        m <- mean(data, ...)#calculate mean using vector data retrieved earlier
        x$setmean(m) #set calculated mean into m the cache
        m #return mean
}
##test the cacheme and makevector
#v1 <- c(1,2,3,4,5)
#vv1 <- makeVector(v1)
#vv1$get()
#vv1$getmean()
#cachemean(vv1)

#//=====matrix inverse==========================
#ref matrix http://www.statmethods.net/advstats/matrix.html
## Put comments here that give an overall description of what your
## functions do

#create 3x3 matrix for testing with solve()
##m1=rbind(c(1, 0, 4), c(1, 3, 4),c(4, 1, 0)) 
##det(m1)
#-48
#matrix with det = 0 means no inverse
##solve(m1)	Inverse of m1 where m1 is a square matrix
##the inverse is
#            [,1]        [,2]    [,3]
#[1,]  0.08333333 -0.08333333  0.2500
#[2,] -0.33333333  0.33333333  0.0000
#[3,]  0.22916667  0.02083333 -0.0625
#//-------------------------------------

## Write a short comment describing this function
#x is square matrix with determinant != 0
#set matrix
#get matrix
#set inverse of matrix
#get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        inv1 <- NULL #set inverse to null for future placeholding
        set <- function(y) { #def a func by lexi scope
                x <<- y #set matrix x with matrix y that in an environment that is different from the current environment.
                inv1 <<- NULL #inv1 reset to null in an environment that is different from current environment
        }
        get <- function() x #get func to return matrix x
        setInv <- function(inverse) inv1 <<- inverse #set function for inverse to be placed in inv1
        getInv <- function() inv1 #get function to return inv1
		
        #return caching of all params defined earlier to working environment
	list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}#end makeCacheMatrix

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        tempInv <- x$getInv() #tempInv will store the inverse cached in x in previous environment
		
	#tempInv is not null, contains cache inverse from previous.
        if(!is.null(tempInv)) {
                message("getting cached data")
                return(tempInv)
        }
		#tempInv is null, create matrix inv2 and store matrix used from previous environment 
        inv2 <- x$get()
        tempInv <- solve(inv2, ...)#calculate inverse using matrix retrieved earlier
        x$setInv(tempInv) #set calculated inverse into tempInv the cache
        tempInv #return inverse
}
##test the cacheSolve and makeCacheMatrix
#m1 <- rbind(c(1, 0, 4), c(1, 3, 4),c(4, 1, 0))
#mm1 <- makeCacheMatrix(m1)
#mm1$get()
#mm1$getInv()
#cacheSolve(mm1)
#=========================================
