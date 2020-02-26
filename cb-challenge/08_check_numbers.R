
## Venu Thatikonda
## Check num1 > num2 or not

check_numbers <- function(num1, num2){
  
  if(num1 > num2){
    
    return(TRUE)
  }
  
  else if(num1 < num2){
    
    return(FALSE)
  }
  else{
    
    return(-1)
  }

}

check_numbers(1, 4)

check_numbers(4, 1)

check_numbers(4, 4)


