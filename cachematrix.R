# This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(beg_mat = matrix())  {
        # set and cache the value of the matrix
        mat <- matrix()
        setMatrix <- function(data)  {
                beg_mat <<- data
                mat <<- matrix()
        }
        # retrieve the value of the matrix
        getMatrix <- function()  beg_mat
        # set and cache the inverted matrix
        setInvMatrix <- function(inv_mat)  mat <<- inv_mat
        # retrieve the value of the inverted matrix
        getInvMatrix <- function()  mat
        # output a list of these functions to be used for calls
        list(set = setMatrix, get = getMatrix,
             setInv = setInvMatrix, getInv = getInvMatrix)
        
}
# This function computes the inverse of the matrix supplied by
# makeCacheMatrix, compares the computed matrix to the inverse matrix
# in cache, and then displays the cache matrix (if there was no change)
# or the calculated inverse which is then saved to cache.
cacheSolve <- function(funcs, ...) {
        # retrieve the matrix and calculate the inverse matrix
        beg_mat <- funcs$get()
        inv_mat <- solve(beg_mat, ...)
        # compare calculated inverse to matrix in cache
        mat <- funcs$getInv()
        if (identical(inv_mat, mat, ignore.environment = TRUE))  {
                # if no change, then return cache inverse
                output = mat
        }  else  {
                # if changed, return calculated inverse and save to cache
                output = inv_mat
                funcs$setInv(inv_mat)
        }
        # show the inverse matrix
        output
}
