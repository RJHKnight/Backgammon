context("Basic test of the backgammon board")
library(Backgammon)
library(tidyverse)

test_that("initial board state is valid", {
  expect_equal(TRUE, validate(getStartingBoard()))
})

test_that("boundary checks for board", {

  initialBoard <- getStartingBoard()

  # White boundary check
  expect_equal(FALSE, canMove(initialBoard, thisColour = WHITE, point = 19, roll = 6))
  expect_equal(TRUE,  canMove(initialBoard, thisColour = WHITE, point = 19, roll = 4))

  # Red boundary check
  expect_equal(FALSE, canMove(initialBoard, thisColour = RED, point = 6, roll = 6))
  expect_equal(TRUE,  canMove(initialBoard, thisColour = RED, point = 6, roll = 4))
})

test_that("valid can moves", {

  initialBoard <- getStartingBoard()

  # White
  expect_equal(FALSE, canMove(initialBoard, thisColour = WHITE, point = 12, roll = 1))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, point = 12, roll = 2))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, point = 12, roll = 3))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, point = 12, roll = 4))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, point = 12, roll = 5))
  expect_equal(TRUE, canMove(initialBoard, thisColour = WHITE, point = 12, roll = 6))

  # Red
  expect_equal(FALSE, canMove(initialBoard, thisColour = RED, point = 13, roll = 1))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, point = 13, roll = 2))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, point = 13, roll = 3))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, point = 13, roll = 4))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, point = 13, roll = 5))
  expect_equal(TRUE, canMove(initialBoard, thisColour = RED, point = 13, roll = 6))

})


test_that("valid moves from bar", {


  initialBoard <- getStartingBoard()

  initialBoard <- initialBoard %>%
    mutate(
      numCheckers = if_else(point == 24, 1, numCheckers)
    ) %>%
    mutate(
      bar = if_else(colour == RED, 1, 0)
    )

  # From bar to point 24
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 1, fromBar = TRUE, point = 0)
  newPoint <- filter(newBoard, point == 24)

  expect_equal(newPoint$numCheckers, 2)
  expect_equal(newPoint$colour, RED)
  expect_equal(newPoint$bar, 0)


  # From bar to point 23
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 2, fromBar = TRUE, point = 0)
  newPoint <- filter(newBoard, point == 24)

  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)
  expect_equal(newPoint$bar, 0)

  newPoint <- filter(newBoard, point == 24)
  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)

  # Invalid move!
  expect_error(newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 6, fromBar = TRUE, point = 0))

  # With blots
  initialBoard <- initialBoard %>%
    mutate(numCheckers = if_else(point == 19, 4, numCheckers)) %>%
    mutate(numCheckers = if_else(point == 20, 1, numCheckers)) %>%
    mutate(colour = if_else(point == 20, WHITE, colour))


  browser()

  # From bar to point 20
  newBoard <- doMove(board = initialBoard, thisColour = RED, roll = 5, fromBar = TRUE, point = 0)
  newPoint <- filter(newBoard, point == 24)

  # Red hits blot on 20
  expect_equal(newPoint$numCheckers, 1)
  expect_equal(newPoint$colour, RED)
  expect_equal(newPoint$bar, 0)

  # And white has a checker on the bar
  newPoint <- filter(newBoard, point == 19)
  expect_equal(newPoint$colour, WHITE)
  expect_equal(newPoint$bar, 1)

})
