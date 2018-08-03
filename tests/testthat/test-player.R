context("player functions")

test_that("NBA scraper", {
  expect_true(all(dim(player_stats("Kobe Bryant", "NBA")) == dim(kb_df)))
  expect_error(player_stats("fjkdl;sa", "NBA"))
})
