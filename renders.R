renderVB <- function(value, subtitle, icon = NULL, color = "blue", width = 4, href = NULL){
  
  renderValueBox({
    valueBox(value = value, subtitle = subtitle, icon = icon, color = color, width = width, href = href)
  })
}


renderOwnDT <- function(x){
  renderDT({x})
}
