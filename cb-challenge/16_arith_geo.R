## Venu Thatikonda
## Check if a given series of numbers follow arithmetic/geometric pattern

## Arithmetic pattern: difference b/w x and x+1 is always constant
## Geometric pattern: result of (x+1)/x is always constant

arith_geo <- function(num_vec){
  
  #num_vec = c(1, 2, 3, 4, 10)
  
  arith_res = diff(num_vec)
  
  ## geometric
  geo_res = NULL
  
  for(i in 1:(length(num_vec)-1)){
    
    first_elm = num_vec[i]
    
    sec_elm = num_vec[i+1]
    
    gr = sec_elm/first_elm
    
    geo_res = c(geo_res, gr)
  }
  
  if(var(arith_res) == 0){
    
    cat("Arithmetic sequence!\n")
    
  } else if(var(geo_res) == 0){
    
    cat("Geometric sequence!\n")
    
  } else {cat("-1\n")}
  
}

arith_geo(num_vec = c(2,6,18))

arith_geo(num_vec = c(1,2,3,4))

arith_geo(num_vec = c(1,6,3,10))