require(rjson)
nextQuestion <- function(userid) {
  list( 
      question =   sprintf("What do you think about %f", rnorm(1)),
      questionid = 1
  );
}
