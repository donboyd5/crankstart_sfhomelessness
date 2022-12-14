
cton <- function(s){
  # character to numeric
  # s: string
  # returns: numeric
  # this works for accounting formats that have parentheses
  # whereas readr::parse_number() does not
  # maybe ifelse using parse_number() unless there is a parenthesis?
  as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", s)))
} 