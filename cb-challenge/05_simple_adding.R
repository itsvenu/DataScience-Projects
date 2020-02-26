## Venu Thatikonda

## adding numbers from 1 to given number
## solving the equation n(n+1)/2, where n= given integer

simple_adding <- function(my_num){
  
  my_num = round(my_num)
  
  res = my_num * (my_num+1)/2
  
  return(res)
  
}

simple_adding(my_num = 10)

simple_adding(my_num = 12.5)
