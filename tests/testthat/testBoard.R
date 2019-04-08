context("Basic test of the backgammon board")
library(Backgammon)

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

test_that("valid single moves", {

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
