test_that("backwards works", {
  bw_anymorts<-readRDS(test_path("fixtures", "bw_anymorts.rds"))
  anymorts<-readRDS(test_path("fixtures", "anymorts.rds"))
  expect_equal(backwards(data=events,morts=anymorts,ID="ID",res.start="ResidenceStart",
                         station="Station.Name",
                         stnchange=NULL),bw_anymorts)
})

test_that("drift works", {
  drift_ex<-readRDS(test_path("fixtures", "drift_ex.rds"))
  expect_equal(drift(data=events[1:10,],type="mort",ID="ID",station="Station.Name",
                     ddd=ddd,from.station="From",to.station="To",
                     verbose=FALSE),drift_ex)
})

test_that("season mm-dd works", {
  ssn_ddmm<-readRDS(test_path("fixtures", "ssn_ddmm.rds"))
  expect_equal(season(data=events,ID="ID",station="Station.Name",
                      season.start="01-06",season.end="31-10",
                      verbose=FALSE),ssn_ddmm)
})

test_that("season Ymd works", {
  ssn_Ymd<-readRDS(test_path("fixtures", "ssn_Ymd.rds"))
  expect_equal(season(data=events,ID="ID",station="Station.Name",
                      season.start=c("2003-06-15","2004-06-21"),
                      season.end=c("2003-10-15","2004-10-30"),
                      verbose=FALSE),ssn_Ymd)
})
