# This is a test function for `cachematrix.R`
# 
# Test init:
# 1. Create the cached matrix
# 2. Calculate its inverse (inv1) --> this should be slow
# 
# Test cases:
# 1. Is the result really an inverse metrix? It should be true that `inv(A)*A = I`.
# 2. Recalculate the inverse (inv2) --> matrix has not changed, it should be
#                    the same as inv1 and the computations should be very fast,
#                    i.e., this is the only case when "getting cached data" appears
# 3. Change the matrix A and recalculate the inverse --> matrix has changed,
#                    however the data are the same, therefore the computation
#                    should be slow, but the result should be the same as inv1
# 4. Change the matrix again, but this time generate new data with a different
#    seed --> matrix has changed, the computation should be slow and the value
#             should be different from inv1
#
testCacheMatrix <- function() {

    source("cachematrix.R");
    
    set.seed(1);
    A <- makeCacheMatrix(matrix(rnorm(100), 10, 10));
    inv1 <- cacheSolve(A);
    if(sum((inv1 %*% A$get()) - diag(10)) > 2e-9) { # 2e-9 == zeroTol
        print("ERROR! inv1 != inverse of A");
    } else {
        print("OK.");
    }
    
    inv2 <- cacheSolve(A);
    if(!identical(inv1, inv2)) {
        print("ERROR! inv1 != inv2");
    } else {
        print("OK.");
    }
    
    A$set(A$get());
    inv3 <- cacheSolve(A);
    if(!identical(inv1, inv3)) {
        print("ERROR! inv1 != inv3");
    } else {
        print("OK.");
    }
    
    set.seed(2);
    A$set(matrix(rnorm(1000), 10, 10));
    inv4 <- cacheSolve(A);
    if(identical(inv1, inv4)) {
        print("ERROR! inv1 == inv3");
    } else {
        print("OK.");
    }

}