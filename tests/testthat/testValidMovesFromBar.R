context("Tests of the calculation of valid moves from various states involving the bar.")


test_that("Multiple points on the bar", {

  bar_board <- getStartingBoard() %>%
    mutate(numCheckers = case_when(
      point == 19        ~ numCheckers - 2,
      point == WHITE_BAR ~ 2,
      TRUE               ~ numCheckers))

  moves <- getAvailableMoves(bar_board, roll = c(2,1), isWhite = TRUE)

  expect_equal(1, nrow(moves))
  expect_equal(WHITE_BAR, moves$point1)
  expect_equal(WHITE_BAR, moves$point2)
})
