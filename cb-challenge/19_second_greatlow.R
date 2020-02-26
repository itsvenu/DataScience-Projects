## Venu Thatikonda
## https://www.coderbyte.com/editor/guest:Second%20GreatLow:Python

## return second lowest, second highest numbers in a given sequence of numbers
## sort them
## print second number from beginning and end

second_greatlow <- function(my_nums){
  
  srt = sort(my_nums)
  
  s_high_idx = length(my_nums)-1
  
  s_high = srt[s_high_idx]
  
  s_low = srt[2]
  
  cat(paste0(s_high, " ", s_low))
  
}

second_greatlow(my_nums = c(2,1,45,10,188))