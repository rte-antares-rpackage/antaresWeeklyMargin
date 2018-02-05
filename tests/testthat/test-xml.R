context("Reading XML files")

test_that("Read hydro usine file with explicit path", {
  usine <- read_hydro_usine(path = test_files("xml_hydro", "PuissancesHydrauliquesHebdo.xml"))
  expect_is(usine, "data.table")
  expect_equal(ncol(usine), 10)
  expect_equal(nrow(usine), 336)
  expect_true(!is.null(usine$datetime))
  expect_is(usine$datetime, "POSIXct")
  expect_true(!is.null(usine$date))
  expect_is(usine$date, "Date")
})


test_that("Read hydro vallee from directory", {
  vallee <- read_hydro_vallee(path = test_files("xml_hydro"))
  expect_is(vallee, "data.table")
  expect_equal(ncol(vallee), 5)
  expect_equal(nrow(vallee), 336)
  expect_true(!is.null(vallee$datetime))
  expect_is(vallee$datetime, "POSIXct")
  expect_true(!is.null(vallee$date))
  expect_is(vallee$date, "Date")
})


test_that("Read thermal tranche from directory", {
  tranche <- read_thermal_tranche(path = test_files("xml_thermal"))
  expect_is(tranche, "data.table")
  expect_equal(ncol(tranche), 7)
  expect_equal(nrow(tranche), 336)
  expect_true(!is.null(tranche$datetime))
  expect_is(tranche$datetime, "POSIXct")
  expect_true(!is.null(tranche$date))
  expect_is(tranche$date, "Date")
})

