# change because NA not in the creating of the SDA data set
# change 7.9.2021 - consider the option that leaders are already written as probabilities

specContrasts <- function(rs,rc,j){
    ## rs = leader of whole sample (as symObject)
    ## rc = leader of one cluster (as symObject)
  mj = length(rs[[j]])  #components + 1
  sizers = rs[[j]][mj]
  sizerc = rc[[j]][mj]
  prs = rs[[j]][-mj]
  if(abs(sum(prs)-1)>10^-8){ ## SE PROBLEM - KAKO RESNO UGOTOVITI, ALI IMAMO PROBABILITIJE???
    prs = prs/sizers # as probability, last is N
  }
  prc = rc[[j]][-mj]
  if(abs(sum(prs)-1)>10^-8){
    prc = prc/sizerc # as probability, last is N
  }
  spec = sum(abs(prs-prc))/2
  contr = prc/prs
  contr = sapply(contr,FUN = function(x)return(ifelse(!is.na(x)&x<1,-x^(-1),x)))
  return(list(specificity = spec, contrasts = contr))
}
  
