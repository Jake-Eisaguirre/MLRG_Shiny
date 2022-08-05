# This is the login credential data for the built in authentication
# NOTE: user and password below are parallel arrays, so the first entry
#   from each array are one login, second entry in each are another login, etc ...
credentials <- data.frame(
  user = c("fake-user-1", "fake-user-2", "fake-user-3"),
  password = c("fake-pw-for-fake-user-1", "fake-pw-for-fake-user-2", "fake-pw-for-fake-user-3"),
  stringsAsFactors = FALSE
)
