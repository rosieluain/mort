test_that("morts method any works", {
  anymorts<-readRDS(test_path("fixtures", "anymorts.rds"))
  expect_equal(morts(data=events,ID="ID",station="Station.Name",
                     method="any",verbose=FALSE),anymorts)
})

test_that("morts method cumulative works", {
  cmlmorts<-readRDS(test_path("fixtures", "cmlmorts.rds"))
  expect_equal(morts(data=events,ID="ID",station="Station.Name",
                     method="cumulative",verbose=FALSE),cmlmorts)
})

test_that("infrequent method recent works", {
  infrmorts<-readRDS(test_path("fixtures", "infrmorts.rds"))
  expect_equal(infrequent(data=events,ID="ID",station="Station.Name",
                          method="recent",threshold=72,threshold.units="hours",
                          recent.period=52,recent.units="weeks",verbose=FALSE),infrmorts)
})

test_that("infrequent method defined works", {
  infdmorts<-readRDS(test_path("fixtures", "infdmorts.rds"))
  expect_equal(infrequent(data=events,type="mort",ID="ID",station="Station.Name",
                          method="defined",threshold=12,threshold.units="hours",
                          start="2006-06-15",end="2006-10-15",verbose=FALSE),infdmorts)
})
