## Venu Thatikonda
## Sum of any combinations of numbers excluding the largest == largest number??

array_addition <- function(num_vec){
  
  num_max = max(num_vec)
  
  num_rem = num_vec[num_vec != num_max]
  
  res = NULL
  
  for(i in 1:length(num_rem)){
    
    rs = combn(num_rem, m = i, FUN=sum)
    
    res = c(res, rs)
    
  }
  
  any(res == num_max)

}

array_addition(num_vec = c(1,2,3,4,5,10))

array_addition(num_vec = c(1,2,3,4,5,100))