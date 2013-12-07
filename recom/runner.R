# 
# runner.R
#
# This script reads a json formatted list of documents from stdin
# and returns a json formated list of recommendations.
# This script is called by drupal to generate input validations and
# recommendations.

require(rjson, quietly=TRUE)
json=fromJSON(file='stdin')
ELTYPE <- readRDS('recom/ELTYPE.cache')
source('recom/allRecoms.R', encoding='utf8')
cat(toJSON(result))



