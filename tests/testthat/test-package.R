context("Package exports and structure")

test_that("Package loads without error", {
  expect_true("hockeystick" %in% loadedNamespaces())
})

test_that("Data getter functions are exported", {
  exported <- getNamespaceExports("hockeystick")
  
  # Check data getters
  expect_true("get_carbon" %in% exported)
  expect_true("get_methane" %in% exported)
  expect_true("get_temp" %in% exported)
  expect_true("get_temp2k" %in% exported)
  expect_true("get_paleo" %in% exported)
  expect_true("get_sealevel" %in% exported)
  expect_true("get_seaice" %in% exported)
  expect_true("get_hurricanes" %in% exported)
  expect_true("get_emissions" %in% exported)
  expect_true("get_dailytemp" %in% exported)
  expect_true("get_icecurves" %in% exported)
  expect_true("get_carbon" %in% exported)
})

test_that("Plot functions are exported", {
  exported <- getNamespaceExports("hockeystick")
  
  expect_true("plot_carbon" %in% exported)
  expect_true("plot_methane" %in% exported)
  expect_true("plot_temp" %in% exported)
  expect_true("plot_temp2k" %in% exported)
  expect_true("plot_paleo" %in% exported)
  expect_true("plot_sealevel" %in% exported)
  expect_true("plot_seaice" %in% exported)
  expect_true("plot_hurricanes" %in% exported)
  expect_true("plot_emissions" %in% exported)
  expect_true("plot_dailytemp" %in% exported)
  expect_true("warming_stripes" %in% exported)
})

test_that("Cache functions are exported", {
  exported <- getNamespaceExports("hockeystick")
  
  expect_true("hockeystick_cache_list" %in% exported)
  expect_true("hockeystick_cache_delete" %in% exported)
  expect_true("hockeystick_cache_delete_all" %in% exported)
  expect_true("hockeystick_cache_details" %in% exported)
  expect_true("hockeystick_update_all" %in% exported)
})

test_that("Helper functions are exported", {
  exported <- getNamespaceExports("hockeystick")
  
  expect_true("merge_carbontemp" %in% exported)
  expect_true("plot_carbontemp" %in% exported)
  expect_true("climate_grid" %in% exported)
  expect_true("emissions_map" %in% exported)
})
