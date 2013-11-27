require(rjson, quietly=TRUE)
json=fromJSON(file='stdin')
ELTYPE <- readRDS('recom/ELTYPE.cache')
source('recom/allRecoms.R')
cat(toJSON(result))



