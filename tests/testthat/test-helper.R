test_that("autofield works", {
  expect_equal(autofield("actel","units"),"secs")
  expect_equal(autofield("glatos","res.start"),"first_detection")
  expect_equal(autofield("mort","res.end",data=events),"ResidenceEnd")
  expect_equal(autofield("vtrack","residences"),"DURATION")
})

test_that("unitcheck works", {
  expect_no_error(unitcheck(type="vtrack",units="secs"))
  expect_warning(unitcheck(type="glatos",units="mins"))
  expect_warning(unitcheck(type="mort",units="secs",data=events))
})

test_that("unitconvert works", {
  expect_equal(unitconvert("secs","mins",60),1)
  expect_equal(unitconvert("mins","secs",60),3600)
  expect_equal(unitconvert("hours","days",72),3)
  expect_equal(unitconvert("days","mins",1),1440)
  expect_equal(unitconvert("weeks","hours",1),168)
})
