#' Get Names of the 12 Notes
#' 
#' Instead of defining a global variable for note names, use this 
#' 
#' @param sharps Whether to describe black keys as sharps or flats (defaults to TRUE)
#' 
#' @return A length-12 character vector containing note names, starting at A, with notes quoted in sharps 
#' 
#' @examples 
#' getNoteNames()
#' 
getNoteNames <- function(sharps = TRUE) {
  if (sharps) {
    return (c("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"))
  }
  return (c("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"))
}

#' Get Numbers of the 12 Notes
#' 
#' Instead of defining a global variable for note numbers, use this 
#' 
#' @return A length-12 integer vector containing note numbers, starting with 0 at A
#' 
#' @examples 
#' getNoteNumbers()
#' 
getNoteNumbers <- function() {
  return (0:11)
}

#' Get Major Scale
#' 
#' Get the numbers of the notes involved in the major scale for a given tonic
#' 
#' @param tonic The number of the note on which the scale begins
#' 
#' @return A length-8 integer vector of the notes involved in the major scale for the given tonic (includes tonic on bottom and top)
#' 
#' @examples
#' getMajorScale(0)
#' getMajorScale(3)
#' getMajorScale(15)
#' 
getMajorScale <- function(tonic) {
  # relative position, in half steps, of notes from tonic
  relatives <- c(0, 2, 4, 5, 7, 9, 11, 12)
  nums <- tonic + relatives
  return (nums %% 12)
}

#' Get Minor Scale
#' 
#' Get the numbers of the notes involved in the minor scale for a given tonic
#' 
#' @param tonic The number of the note on which the scale begins
#' 
#' @return A length-8 integer vector of the notes involved in the minor scale for the given tonic (includes tonic on bottom and top)
#' 
#' @examples
#' getMinorScale(0)
#' getMinorScale(3)
#' getMinorScale(15)
#' 
getMinorScale <- function(tonic) {
  # relative position, in half steps, of notes from tonic
  relatives <- c(0, 2, 3, 5, 7, 8, 10, 12)
  nums <- tonic + relatives
  return (nums %% 12)
}

#' Get Scale
#' 
#' Get the notes involved in the given scale type for a given tonic
#' 
#' @param tonic The name of the note on which the scale begins
#' @param scale_type The character string describing the type of scale. Options are "M", "maj", or "major" for major, "m", "min", or "minor" for minor
#' 
#' @return A length-8 character vector of the notes involved in the scale (tonic on bottom and top)
#' 
#' @examples 
#' getScale("A", "M")
#' getScale("G#", "major")
#' getScale("Ab", "minor")
#' 
#' @export
#' 
getScale <- function(tonic, scale_type) {
  if (grepl("b", tonic)) {
    notes <- getNoteNames(sharps = FALSE)
  } else {
    notes <- getNoteNames(sharps = TRUE)
  }
  nums <- getNoteNumbers()
  tonic_num <- nums[notes == tonic]
  if (scale_type %in% c("major", "M", "maj")) {
    scale_nums <- getMajorScale(tonic_num)
  } else if (scale_type %in% c("minor", "m", "min")) {
    scale_nums <- getMinorScale(tonic_num)
  } else {
    stop ("scale_type not recognized")
  }
  return (notes[scale_nums + 1])
}


