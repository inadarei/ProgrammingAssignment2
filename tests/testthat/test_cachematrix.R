# Tests for cachematrix
# For using testthis framework, see: http://r-pkgs.had.co.nz/tests.html

context("Assignment 2: Cacheable Matrix")

source ("../../cachematrix.R")
  
## Compare two matrices
matrices_equal <- function(x, y) {
  ## Returns true if x and y are "equal" matrices
  is.matrix(x) && is.matrix(y) && (dim(x) == dim(y)) && all(x == y)
}

test_that("Matrix reversed properly", {
  
  random_matrix <- matrix(sample(300,25,T),5)
  
  rev_direct_test         <- solve(random_matrix)
  cacheable_matrix        <- makeCacheMatrix(random_matrix)
  reversed                <- cacheSolve(cacheable_matrix)
  expect_that(matrices_equal(reversed, rev_direct_test), equals(T))
  
  # If we run it second time, we should get cached data!
  expect_message(reversed <- cacheSolve(cacheable_matrix), "getting cached data")
  expect_that(matrices_equal(reversed, rev_direct_test), equals(T))
  
  # Unless data is modified:
  random_matrix2          <- matrix(sample(300,16,T),4)
  rev_direct_test         <- solve(random_matrix2)
  cacheable_matrix        <- makeCacheMatrix(random_matrix2)
  expect_silent(reversed  <- cacheSolve(cacheable_matrix))
  expect_that(matrices_equal(reversed, rev_direct_test), equals(T))
  # If we run it second time, we should get cached data!
  expect_message(reversed <- cacheSolve(cacheable_matrix), "getting cached data")
  expect_that(matrices_equal(reversed, rev_direct_test), equals(T))
  
  
  # print(random_matrix)
  
})