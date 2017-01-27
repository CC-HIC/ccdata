library(ccdata)
library(rmongodb)
library(assertthat)
mongo <- mongo.create()
assert_that(mongo.is.connected(mongo))
# how many databases
print(mongo.get.databases(mongo))
coll <- "ccd.ccd1"

#for_each_episode(ccd_delta_num, 
#                 function(x) {
#                     mongo.insert(mongo, coll, mongo.bson.from.list(x@data))
#                 })
