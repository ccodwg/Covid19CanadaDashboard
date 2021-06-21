preloader_spinner <- function() {
  
  list(html = waiter::spin_5())
  
}

fresh_theme <- function() {
  
  fresh::create_theme(
    
    fresh::bs4dash_font(
      family_sans_serif = "Lato"
    )
    
  )
  
}