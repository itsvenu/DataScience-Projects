## Venu Thatikonda
## True if given string is palindrome

check_palindrome <- function(my_str){
  
  my_str_rev = reverse(my_str)
  
  if(my_str == my_str_rev){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

check_palindrome('venu')

check_palindrome('eye')