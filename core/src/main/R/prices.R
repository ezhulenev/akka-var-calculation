require(quantmod)

sp500Symbols <- read.csv("sp500.csv", header = F, stringsAsFactors = F)[,1]

getSymbols(sp500Symbols[310:397])

invisible(lapply(sp500Symbols, function(s) {
  if (exists(s)) {
    xts <- get(s)
    csv <- paste(s, ".csv", sep = "")
    write.csv(as.data.frame(xts), csv)
  }
}))
