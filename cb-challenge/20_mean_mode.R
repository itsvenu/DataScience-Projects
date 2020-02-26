## Venu Thatikonda
## https://www.coderbyte.com/editor/guest:Mean%20Mode:Python

## check if mean == mode in a given arr

mean_mode <- function(my_nums){
  
  my_mean = mean(my_nums)
  
  my_mode = mode(my_nums)

  if(my_mean == my_mode){
    
    return(1)
    
  }else{
    
    return(0)
    
  } 
  
}

mean_mode(c(1,2,2,2,2,44,5))