# Functions for manuipulation of the board
WHITE <- 0
RED <- 1


getStartingBoard <- function() {

  board <- tibble(
    point = 1:24,
    numCheckers = c(2,0,0,0,0,5,0,3,0,0,0,5,5,0,0,0,3,0,5,0,0,0,0,2),
    colour = c(WHITE,NA,NA,NA,NA,RED,NA,RED,NA,NA,NA,WHITE,RED,NA,NA,NA,WHITE,NA,WHITE,NA,NA,NA,NA,RED),
    bar = 0
  )

  return (board)
}

isWhite <- function(colour) {
  return (colour == WHITE)
}

getOtherColour <- function(colour) {
  return (if_else(isWhite(colour), RED, WHITE))
}

hasCheckerOnBar <- function(board, thisColour) {

  return (getNumOnBar(board, thisColour) > 0)
}

getNumOnBar <- function(board, thisColour){
  return (board %>%
            filter(colour == thisColour) %>%
            group_by(colour) %>%
            summarise(numOnBar = max(bar)) %>%
            ungroup() %>%
            pull(numOnBar)
  )
}

validate <- function(board) {

  numByColour <- board %>%
    filter(!is.na(colour)) %>%
    group_by(colour) %>%
    summarise(sum = sum(numCheckers) + min(bar)) %>%
    ungroup()

  return (all(pull(numByColour, sum) == 15))
}

getValue <- function(board, thisPoint, number) {

  return (
    board %>%
      filter(point == thisPoint) %>%
      mutate(indicator = case_when(
        colour == RED & numCheckers >= number ~ " x",
        colour == WHITE & numCheckers >= number ~ " o",
        TRUE ~ "  "
      )) %>%
      pull(indicator)
  )
}

pad <- function(number) {

  padOne <- function(x) {
    if (x < 10)
      return (paste(" ", x, sep = ""))
    else {
      return (as.character(x))
    }
  }

  return (sapply(number, padOne))
}

printBoard <- function(board) {

  leftSide <- paste(13:18, collapse = " ")
  rightSide <- paste(19:24, collapse = " ")

  boardString <- paste("", leftSide, "|", rightSide, "\n")

  # Top
  for (row in 1:5) {
    for (point in 13:18) {
      boardString <- paste(boardString, getValue(board, point, row))
    }

    boardString <- paste(boardString, "|")

    for (point in 19:24) {
      boardString <- paste(boardString, getValue(board, point, row))
    }

    boardString <- paste(boardString, "\n", sep = "")
  }


  # Middle
  boardString <- paste(boardString, "\n", sep = "")
  boardString <- paste(boardString, "\n", sep = "")


  for (row in 5:1) {

    for (point in 12:7) {
      boardString <- paste(boardString, getValue(board, point, row))
    }

    boardString <- paste(boardString, "|")

    for (point in 6:1) {
      boardString <- paste(boardString, getValue(board, point, row))
    }

    boardString <- paste(boardString, "\n", sep = "")
  }

  leftSide <- paste(pad(12:7), collapse = " ")
  rightSide <- paste(pad(6:1), collapse = " ")

  bottomString <- paste("", leftSide, "|", rightSide, "\n")

  boardString <- paste(boardString, bottomString, sep = "")

  return (boardString)
}
