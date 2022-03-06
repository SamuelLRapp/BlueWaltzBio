# parse a csv file and append the list of organism
# names to what's already in the text box
parseCsvColumnForTxtBox <- function(input, file.index, column.header, textbox.id) {
  req(input[[file.index]],
      file.exists(input[[file.index]]$datapath))
  # read.csv complains about there being an incomplete final line (no new line character)
  # if (readLines(input[[file.index]]$datapath, -1) != "\n"){
  #  write.table("\n", input[[file.index]]$datapath, append=TRUE, col.names = FALSE)
  #}
  uploadinfo <-
    read.csv(input[[file.index]]$datapath, header = TRUE)
  # preserve any organism names already in the text box
  if (input[[textbox.id]][[1]] == "") {
    return(uploadinfo[[column.header]][uploadinfo[[column.header]] != ""])
  }
  c(input[[textbox.id]], head(uploadinfo[[column.header]][uploadinfo[[column.header]] != ""]))
}

