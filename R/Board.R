
# Calculate all possible moves
getAvailableMoves <- function(board, roll, isWhite) {

  thisColour <- if_else(isWhite, WHITE, RED)
  d1 <- roll[1]
  d2 <- roll[2]

  if (d1 == d2) {
    # Doubles...
  }

  onBar <- board %>%
    filter(point == getBarPoint(thisColour)) %>%
    pull(numCheckers)

  if (anyBar) {

  }

  else {

    myPoints <- board %>%
      filter(colour == thisColour & numCheckers > 0) %>%
      pull(point)

    # First moving a checker twice
    sameChecker <- expand.grid(point1 = myPoints, roll1 = c(d1, d2), roll2 = c(d2, d1)) %>%
      filter(!roll1 == roll2) %>%
      mutate(point2 = if_else(rep(isWhite(thisColour), length(point1)), point1 + roll1, point1 - roll1))

    # Then moving two different checkers
    differentCheckers <-
      expand.grid(point1 = myPoints, point2 = myPoints, roll1 = c(d1, d2), roll2 = c(d1, d2)) %>%
      filter(!roll1 == roll2 & !point1 == point2)

    allowedMoves <- rbind(sameChecker, differentCheckers) %>%
      rowwise() %>%
      mutate(allowed = checkOneRoll(board, thisColour, point1, point2, roll1, roll2)) %>%
      filter(allowed)

    return (allowedMoves)
  }

}

checkOneRoll <- function(board, thisColour, p1, p2, r1, r2) {

  m1 <- canMove(board, thisColour, p1, r1)

  if (!m1)
    return (FALSE)

  b2 <- doMove(board, thisColour, p1, r1)

  m2 <- canMove(b2, thisColour, p2, r2)

  return (m2)
}

canMove <- function(board, thisColour, thisPoint, roll) {

  newPoint <- if_else(isWhite(thisColour), thisPoint + roll,thisPoint - roll)

  # Boundary check
  if (newPoint > 24 | newPoint < 1)
    return (FALSE)

  # Check that we are moving our piece!
  ourPiece = board %>%
    filter(point == thisPoint) %>%
    mutate(ourPiece = colour == thisColour) %>%
    pull(ourPiece)

  if (!ourPiece) {
    return (FALSE)
  }

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
      mutate(numCheckers = case_when(
        point == newPoint & numCheckers == 0     ~ 1,                   # Nothing on the new point
        point == newPoint & colour == thisColour ~ numCheckers + 1,     # Adding to our colour
        point == newPoint & colour != thisColour ~ -1,                  # Hit
        TRUE ~ numCheckers
      )) %>%
      mutate(colour = if_else(point == newPoint, thisColour, colour)) %>%
      mutate(numCheckers = if_else(point == getBarPoint(thisColour), numCheckers-1, numCheckers))

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
    mutate(numCheckers = if_else(point == pointToResolve, 1, numCheckers)) %>%
    mutate(numCheckers = if_else(point == getBarPoint(colourToResolve), numCheckers + 1, numCheckers))

  boardOK <- validate(newBoard)

  if (boardOK)
    return (newBoard)

  stop("Invalid board!!!!")

}
