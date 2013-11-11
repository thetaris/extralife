require(rjson, quietly=TRUE)
json=fromJSON(file='stdin')

result= data.frame()

for (i in 1:length(json)) {
  node <- json[[i]]
  if (sum(node$person.geburtsdatum=="")) {
      result <- rbind(result, data.frame(
        recom_id = 442,
        type = "NODE",
        target_id = node$node_id,
        is_satisfied = FALSE
      ))
  } 
}

result <- rbind(result, data.frame(
  recom_id = 505,
  type = "TERM",
  target_id = "266",
  is_satisfied = FALSE
))

result <- rbind(result, data.frame(
  recom_id = 628,
  type = "TERM",
  target_id = "305",
  is_satisfied = FALSE
))

cat(toJSON(result))



