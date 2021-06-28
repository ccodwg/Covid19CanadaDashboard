format_number <- function(.number) {
  
  if (is.na(.number)) {
    
    "No data available"
    
  } else if (.number < 1000) {
    
    round(.number) |> 
      as.character()
    
  } else if (.number < 100000) {
    
    {.number / 1000} |> 
    round(digits = 2) |> 
    paste0("k")
    
  } else if (.number < 1000000) {
    
    {.number / 1000} |> 
    round(digits = 1) |> 
    paste0("k")
    
  } else {
    
    {.number / 1000000} |> 
    round(digits = 2) |> 
    paste0("mm")
    
  }
  
}

format_value_html <- function(.number) {
  
  gen_html <- function(.number, .unit) {
    
    div(
      h1(
        paste0(.number, " "), 
        style = "display: inline;"
      ),
      h5(
        .unit, 
        style = "display: inline;"
      )
    )
    
  }
  
  if (is.na(.number)) {
    
    h3("No data")
    
  } else if (.number < 1000) {
    
    round(.number) |> 
      as.character() |> 
      h1()
    
  } else if (.number < 100000) {
    
    {.number / 1000} |> 
      round(digits = 2) |> 
      gen_html("thousand")
    
  } else if (.number < 1000000) {
    
    {.number / 1000} |> 
      round(digits = 1) |> 
      gen_html("thousand")
    
  } else {
    
    {.number / 1000000} |> 
      round(digits = 2) |> 
      gen_html("million")
    
  }
  
}

format_secondary <- function(.number, .secondary_type, .color) {
  
  if (.secondary_type == "new") {
    
    secondary_formatted <- format_new(.number)
    
  } else if (.secondary_type == "change") {
    
    secondary_formatted <- format_change(.number)
    
  } else {
    
    stop("Invalid value of .secondary_type")
    
  }
  
  if (.color == "white") {
        
    div(
      secondary_formatted, 
      style = "color: black;"
    )
    
  } else {
    
    div(secondary_formatted)
    
  }
  
}

format_change <- function(.number) {
  
  number_formatted <- format_number(.number)
  
  paste0(
    "Change: ",
    {if (.number > 0) "+" else ""},
    number_formatted
  )
  
}

format_new <- function(.number) {
  
  number_formatted <- format_number(.number)
  
  paste0(
    number_formatted,
    " new"
  )
  
}