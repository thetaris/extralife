family1 <- data.frame(
  name = c("Stefan", "Qiu", "Inika","smith"),
  birthYear = c(1975, 1971, 2011, 2013),
  # 1 for female, 2 for male, 3 for na
  sex = c(1, 2, 2,3)
  )

plotFamilyMort(family1)