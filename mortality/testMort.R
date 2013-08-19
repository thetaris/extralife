family1 <- data.frame(
  name = c("Stefan", "Qiu", "Inika","Simon"),
  birthYear = c(1975, 1971, 2011, 1987),
  # 1 for female, 2 for male, 3 for na
  sex = c(1, 2, 2,0)
  )

plotFamilyMort(family1)