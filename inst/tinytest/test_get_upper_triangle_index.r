## -------->>  [[file:../../disambr.src.org::*get_upper_triangle_index][get_upper_triangle_index:2]]
get_upper_triangle_index <- disambr:::get_upper_triangle_index

## 3x3
m <- matrix(c(1,2,3
             ,0,4,5
             ,0,0,6)
          , nrow = 3
          , byrow = TRUE)

i <- 2
j <- 1

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)


i <- 3
j <- 3

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)


i <- 3
j <- 2

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)

## 4x4
m <- matrix(c(1,2,3,4
             ,0,5,6,7
             ,0,0,8,9
             ,0,0,0,10)
          , nrow = 4
          , byrow = TRUE)


i <- 2
j <- 1

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)


i <- 3
j <- 3

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)


i <- 3
j <- 2

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)



i <- 4
j <- 2

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)


i <- 1
j <- 4

expect_equal(
    m[i,j]
  , get_upper_triangle_index(i,j,dim(m)[1])
)
## --------<<  get_upper_triangle_index:2 ends here


