
# Calculate all possible moves
getAvailableMoves <- function(board, roll, isWhite) {

  thisColour <- if_else(isWhite, WHITE, RED)
  d1 <- roll[1]
  d2 <- roll[2]

  if (d1 == d2) {
    # Doubles...
  }

  myBoard <- board %>%
    filter(colour == thisColour)

  onBar <- myBoard %>%
    summarise(bar = max(bar)) %>%
    mutate(anyBar = bar > 0) %>%
    pull(anybar)

  if (anyBar) {

  }

  else {

    myBoard %>%
      group_by(point)


  }

}

canMove <- function(board, thisColour, point, roll) {

  newPoint <- if_else(isWhite(thisColour), point + roll,point - roll)

  # Boundary check
  if (newPoint > 24 | newPoint < 1)
    return (FALSE)

  return (board %>%
            filter(point == newPoint) %>%
            mutate(allowed = is.na(colour) | (colour == thisColour) | (numCheckers == 1)) %>%
            pull(allowed)
  )
}

doMove <- function(board, thisColour, thisPoint, roll, fromBar = FALSE) {

  if (fromBar) {

    newPoint <- if_else(isWhite(thisColour), roll, 25-roll)

    newBoard <- board %>%
      mutate(bar = if_else(colour == thisColour, bar-1, bar)) %>%
      mutate(numCheckers = case_when(
        point == newPoint & numCheckers == 0     ~ 1,                   # Nothing on the new point
        point == newPoint & colour == thisColour ~ numCheckers + 1,     # Adding to our colour
        point == newPoint & colour != thisColour ~ -1,                  # Hit
        TRUE ~ numCheckers
      )) %>%
      mutate(
        colour = if_else(point == newPoint, thisColour, colour)
      )

  }
  else {

    newPoint <- if_else(isWhite(thisColour), thisPoint+roll, thisPoint-roll)

    newBoard <- board %>%
      mutate(numCheckers = case_when(
        point == newPoint & numCheckers == 0     ~ 1,                   # Nothing on the new point
        point == newPoint & colour == thisColour ~ numCheckers + 1,     # Adding to our colour
        point == newPoint & colour != thisColour ~ -1,                  # Hit
        point == thisPoint ~ numCheckers -1,
        TRUE ~ numCheckers
      )) %>%
      mutate(
        colour = if_else(point == newPoint, thisColour, colour)
      )

  }

  newBoard <- resolveHits(newBoard)
  return (newBoard)
}

resolveHits <- function(board) {

  toResolve <- board %>%
    filter(numCheckers < 0)

  # Nothing to resolve.
  if (nrow(toResolve) == 0) {
    return (board)
  }

  pointToResolve <- toResolve$point[1]
  colourToResolve <- getOtherColour(toResolve$colour[1])

  newBoard <- board %>%
    mutate(
      numCheckers = if_else(point == pointToResolve, 1, numCheckers),
      bar = if_else(colour == colourToResolve, bar + 1, bar)
    )

  boardOK <- validate(newBoard)

  if (boardOK)
    return (newBoard)

  stop("Invalid board!!!!")

}
