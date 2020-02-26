## Venu Thatikonda

## given number of seconds, return in hours/minutes

time_convert <- function(my_sec){
  
  my_h = round(my_sec / 60)
  
  my_min = my_sec %% 60
  
  cat(paste0(my_h, ":", my_min))
  
}

time_convert(my_sec = 63)

time_convert(my_sec = 38266182047)