kbvec <- which(tblAggression[,4] == "kb")
otvec <- which(tblAggression[,4] == "ot")
print(which(tblAggression[kbvec,5] == "ot"))
print(which(tblAggression[otvec,5] == "kb"))