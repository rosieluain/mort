test_that("review works", {
  review_newdata<-readRDS(test_path("fixtures", "review_newdata.rds"))
  anymorts<-readRDS(test_path("fixtures", "anymorts.rds"))
  expect_equal(review(morts=anymorts,new.data=new.data,type="mort",ID="ID",
                      station="Station.Name"),review_newdata)
})
