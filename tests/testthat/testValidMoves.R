context("Tests of the calculation of valid moves from various states.")

test_that("valid moves from initial states - non double", {
  initial_board <- getStartingBoard()

  # 6,5 - White
  roll <- c(6,5)
  moves <- getAvailableMoves(initial_board, roll, TRUE)
  expect_equal(15, nrow(moves))
  expect_equal(1, nrow(filter(moves, point1 == 1 & point2 == 7)))

  # 6,5 - RED
  roll <- c(6,5)
  moves <- getAvailableMoves(initial_board, roll, FALSE)
  expect_equal(15, nrow(moves))
  expect_equal(1, nrow(filter(moves, point1 == 24 & point2 == 18)))

  # 6,4 - White
  roll <- c(6,4)
  moves <- getAvailableMoves(initial_board, roll, TRUE)
  expect_equal(28, nrow(moves))
  expect_equal(1, nrow(filter(moves, point1 == 1 & point2 == 7)))

})

test_that("valid moves from synthetic states", {

  initial_board <- getStartingBoard()

  # Set to a artifically limited state
  synth_board <- initial_board %>%
    mutate(numCheckers = case_when(
      point == 19 ~ 15,
      point == 6  ~ 15,
      TRUE        ~ 0
    )) %>%
    mutate(colour = case_when(
      point == 19 ~ WHITE,
      point == 6  ~ RED,
      TRUE        ~ NA_real_
    ))

  # 6,5 - White
  roll <- c(5,4)
  moves <- getAvailableMoves(synth_board, roll, TRUE)
  expect_equal(2, nrow(moves))
  expect_equal(2, nrow(filter(moves, point1 == 19 & point2 == 19)))

})
