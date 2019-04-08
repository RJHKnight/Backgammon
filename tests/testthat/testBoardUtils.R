context("Basic test of the backgammon board utils")
library(Backgammon)

test_that("validate bar checks", {

  initialBoard <- getStartingBoard()

  # Set up a dummy board where RED has 1 checker on the bar
  newBoard <- initialBoard %>%
    mutate(
      numCheckers = if_else(point == 24, 1, numCheckers)
    ) %>%
    mutate(
      bar = if_else(colour == RED, 1, 0)
    )

  # With initial board, nothing on the bar
  expect_equal(FALSE, hasCheckerOnBar(initialBoard, RED))
  expect_equal(0, getNumOnBar(initialBoard, RED))
  expect_equal(FALSE, hasCheckerOnBar(initialBoard, WHITE))
  expect_equal(0, getNumOnBar(initialBoard, WHITE))

  # And with modified board, RED should have 1 on the bar
  expect_equal(TRUE, hasCheckerOnBar(newBoard, RED))
  expect_equal(1, getNumOnBar(newBoard, RED))
  expect_equal(FALSE, hasCheckerOnBar(newBoard, WHITE))
  expect_equal(0, getNumOnBar(newBoard, WHITE))

})


test_that("flip colour", {

  expect_equal(getOtherColour(WHITE), RED)
  expect_equal(getOtherColour(RED), WHITE)
})

