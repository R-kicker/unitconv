test_that("basic unit conversion works", {

  expect_error(uc(1, "kg/m3", "lbft3"))

  expect_lte(abs(uc(1, "mkm2", "D") - 1.013274), 1e-6)

  expect_lte(abs(uc(uc(uc(1, "ft3", "l"), "l", "cm3"), "cm3", "bbl")) - 0.1781, 1e-5)

  expect_equivalent(uc(uc(u(list(1, 2), "ton"), "ton", "lb"), "lb", "ton"),
                    u(list(1, 2), "ton"))

})
