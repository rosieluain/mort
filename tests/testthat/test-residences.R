test_that("residences work", {
  expect_equal(residences(data=detections[1:6564,],ID="ID",station="Station.Name",
                          datetime="DateTimeUTC",cutoff=1,units="days",
                          verbose=FALSE), events[1:3,])
})
