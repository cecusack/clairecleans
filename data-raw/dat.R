## code to prepare `dat` dataset goes here

usethis::use_data(dat, overwrite = TRUE)
# create empty dataframe
dat <- data.frame(id = rep(NA, 17),
                  age = rep(NA, 17),
                  weight = rep(NA, 17),
                  height = rep(NA, 17))
# make up some fake data
dat$id <- seq(1:nrow(dat)) # just number 1 through however many rows there are
set.seed(1234) # set seed to make line below reproducible
dat$age <- sample(c(18:30), size = 17, replace = TRUE) # randomly sample between the numbers 18-30, 15 times, can reuse values
dat$weight <- c("42 kg", "110 lbs", "178.5", "65 kilograms", "215 POUNDS", "182", "135.2 lbs.", "83", "70 KG", "274", "105lb", "150 POUNDS", "170 pounds", "130",  "130", "seventy seven", "13 stones") # writing common ways I've seen weight
dat$height <- c("five foot three", "five feet 7 inches", "five ft 9 in", "5 feet 5 inches", "5'8", "five foot 6", "5'4", "5'1.5", "182 cm", "5'10''", "5", "5 feet", "5 8", "176", "5'2.5'", "30", "1234") # writing common ways I've seen height
