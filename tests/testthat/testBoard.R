context("Basic test of the backgammon board")

test_that("initial board state is valid", {
  expect_equal(TRUE, validate(getStartingBoard()))
})

test_that("boundary checks for board", {

  initialBoard <- getStartingBoard()

  # White boundary check
  expect_equal(FALSE, canMove(initialBoard, thisColour = WHITE, thisPoint = 19, roll = 6))
  expect_equal(TRUE,  canMove(initialBoard, thisColour = WHITE, thisPoint = 19, roll = 4))

  # Red boundary check
  expect_equal(FALSE, canMove(initialBoard, thisColour = RED, thisPoint = 6, roll = 6))
  expect_equal(TRUE,  canMove(initialBoard, thisColour = RED, thisPoint = 6, roll = 4))
})

test_that("valid can moves", {

  initialBoard <- getStartingBoard()

  # White
  expect_equal(FALSE, canMove(initialBoard, thisColour = WHITE, thisPoint = 12, roll = 1))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, thisPoint = 12, roll = 2))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, thisPoint = 12, roll = 3))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, thisPoint = 12, roll = 4))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, thisPoint = 12, roll = 5))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, thisPoint = 12, roll = 6))

  # Red
  expect_equal(FALSE, canMove(initialBoard, thisColour = RED, thisPoint = 13, roll = 1))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, thisPoint = 13, roll = 2))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, thisPoint = 13, roll = 3))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, thisPoint = 13, roll = 4))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, thisPoint = 13, roll = 5))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, thisPoint = 13, roll = 6))

})


test_that("valid moves from bar", {

  initialBoard <- getStartingBoard()

  initialBoard <- initialBoard %>%
    mutate(
      numCheckers = if_else(point == 24, 1, numCheckers)
    ) %>%
    mutate(
      numCheckers = if_else(point == RED_BAR, 1, numCheckers)
    )

  # From bar to point 24
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 1, fromBar = TRUE, thisPoint = 0)
  newPoint <- filter(newBoard, point == 24)
  newBar <- filter(newBoard, point == RED_BAR)

  expect_equal(newPoint$numCheckers, 2)
  expect_equal(newPoint$colour, RED)
  expect_equal(newBar$numCheckers, 0)


  # From bar to point 23
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 2, fromBar = TRUE, thisPoint = 0)
  newPoint <- filter(newBoard, point == 24)
  newBar <- filter(newBoard, point == RED_BAR)

  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)
  expect_equal(newBar$numCheckers, 0)

  newPoint <- filter(newBoard, point == 24)
  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)

  # Invalid move!
  expect_error(newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 6, fromBar = TRUE, thisPoint = 0))

  # With blots
  initialBoard <- initialBoard %>%
    mutate(numCheckers = if_else(point == 19, 4, numCheckers)) %>%
    mutate(numCheckers = if_else(point == 20, 1, numCheckers)) %>%
    mutate(colour = if_else(point == 20, WHITE, colour)) %>%
    mutate(numCheckers = if_else(point == getBarPoint(RED), 1, numCheckers))

  printBoard(initialBoard)

  browser()

  # From bar to point 20
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 5, fromBar = TRUE, thisPoint = 0)

  # Red hits blot on 20
  newPoint <- filter(newBoard, point == 20)
  whiteBar <- filter(newBoard, point == getBarPoint(WHITE))
  redBar <- filter(newBoard, point == getBarPoint(RED))

  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)
  expect_equal(redBar$numCheckers, 0)

  # And white has a checker on the bar
  expect_equal(whiteBar$numCheckers, 1)

})


test_that("valid moves from board", {

  initialBoard <- getStartingBoard()

  # Lovers leap for red - (6-5 opening)
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 6, thisPoint = 24)
  newBoard <- doMove(board = newBoard, thisColour = RED, roll = 5, thisPoint = 18)

  newPoint <- filter(newBoard, point == 24)
  redBar <- filter(newBoard, point == getBarPoint(RED))

  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)
  expect_equal(redBar$numCheckers, 0)

  newPoint <- filter(newBoard, point == 13)
  expect_equal(newPoint$numCheckers, 6)
  expect_equal(newPoint$colour, RED)


  # White not so lucky - (5-1) split
  newBoard <- doMove(board = newBoard, thisColour = WHITE, roll = 5, thisPoint = 12)
  newBoard <- doMove(board = newBoard, thisColour = WHITE, roll = 1, thisPoint = 1)


  newPoint <- filter(newBoard, point == 17)
  whiteBar <- filter(newBoard, point == getBarPoint(WHITE))
  expect_equal(newPoint$numCheckers, 4)
  expect_equal(newPoint$colour, WHITE)
  expect_equal(whiteBar$numCheckers, 0)

  newPoint <- filter(newBoard, point == 1)
  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, WHITE)

  newPoint <- filter(newBoard, point == 2)
  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, WHITE)

})

