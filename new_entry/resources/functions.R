text_combination_generator <- function(n = 1,length = 10){
  
  sapply(1:n,function(x) paste0(
    sample(
      c(LETTERS,
        letters,
        0:9),
      size = length,
      replace = TRUE
    ),
    collapse = ""
  )
  )
}