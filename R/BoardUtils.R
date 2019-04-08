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

