apa_number <- function(val, P=FALSE, above_zero = FALSE, na.rm = TRUE){
# Adapted from: https://github.com/hollzzar/manuscript-template/blob/main/sections/code/scripts/source_script.R (hollzzar, retrieved at 14-08-2025)
  
  if(P==TRUE) { 
    val <- if_else(val < 0.001, "<.001", substring(sprintf("%.3f", round(val, 3)), 2))
  } else { 
    if(above_zero == TRUE){
      val <- if_else(val < 0.01, "<0.01", sprintf("%.2f", round(val, 3)))  
    } else {
      if(val < 1){
        val <- if_else(val < 0.01, "<.01", substring(sprintf("%.2f", round(val, 3)), 2))
      } else { 
        val<- sprintf("%.2f", round(val, 2))
      }
    }
  }
  return(val)
  
} # End apa_number

apa_p <- function(val){ val <- if_else(val < 0.001, "<.001", substring(sprintf("%.3f", round(val, 3)), 2))}
apa_above0 <-function(val){ val <- if_else(val < 0.01, "<0.01", sprintf("%.2f", round(val, 3)))}
apa_below0 <- function(val){ val <- if_else(val < 0.01, "<.01", substring(sprintf("%.2f", round(val, 3)), 2))}