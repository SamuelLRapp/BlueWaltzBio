
import(shiny)

# pass in the text that you want the tooltip to display
infoIcon = function(text) {
  return (
    tags$div(class = "info-tooltip",
      tags$span(class = "info-icon", "?"),
      tags$span(class = "info-tooltip-text", text)         
    )
  );
}