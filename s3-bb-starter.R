#TODO:
#check_assert typen anpassen
#factor anpassen


#Konstruktorfunktion
new_bb <- function(text) {
  checkmate::assert_character(text) #integrieren von alles typen???
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  structure(gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text), class = "bb")
}
texttest <- "Bedeutet nach jedem Vokal oder Diphtong die Konsonanten..."
new_bb(texttest)

#Validatorfunktion

validate_bb <- function(text) {
  checkmate::assert_class(text, "bb")
  text
}

#neu generics definition

bb <- function(text, ...) UseMethod("bb")

#neu methods

bb.default <- function(text, ...) {
  validate_bb(new_bb(text)) #interface
}

bb.list <- function(text, ...) {
  checkmate::assert_character(unlist(text)) #oder nach list checken
  text <- rapply(text, bb, how = "list")
  class(text) <- append(class(text), "bb")
  text
}

bb.factor <- function(text, ...) {
  levels(text) <- bb(levels(text))
  class(text) <- append(class(text), "bb")
  text
}

