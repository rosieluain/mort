test_that("stationchange works", {
  stnchange<-readRDS(test_path("fixtures", "stnchange.rds"))
  expect_equal(stationchange(data=events,ID="ID",station="Station.Name"),
               stnchange)
})

test_that("resmax works", {
  maxres<-readRDS(test_path("fixtures", "maxres.rds"))
  stnchange<-readRDS(test_path("fixtures", "stnchange.rds"))
  expect_equal(resmax(data=events,ID="ID",station="Station.Name",
                      res.start="ResidenceStart",residences="ResidenceLength.days",
                      stnchange=stnchange),
               maxres)
})

test_that("resmaxcml works", {
  maxrescml<-readRDS(test_path("fixtures", "maxrescml.rds"))
  stnchange<-readRDS(test_path("fixtures", "stnchange.rds"))
  expect_equal(maxrescml<-resmaxcml(data=events[events$ID=="A",],ID="ID",station="Station.Name",
                                    res.start="ResidenceStart",res.end="ResidenceEnd",
                                    residences="ResidenceLength.days",
                                    units="days",stnchange=stnchange),
               maxrescml)
})
