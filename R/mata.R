#https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/topo/vehicules?_tmp=1647467979421
#https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/horaires/pta/1427098 particular station message or schedule?
#https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/topo/refresh?_tmp=1647400000 _tmp is unix epoch
#https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/topo
#https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/iv/message
#https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/config/version?_tmp=1647465918008

tmp <- paste0(as.character(round(as.numeric(Sys.time()))), "000")
load("data/stop.id.RData")
library(lubridate)

tryCatch(download.file(paste0("https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/topo/vehicules?_tmp=", tmp), destfile = paste0("data/mata/", "vehicules-", make.names(Sys.time()), "-", tmp, ".json")),
         error = function(e) print(paste("vehicules", 'did not work out')))

tryCatch(download.file(paste0("https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/topo/refresh?_tmp=", tmp), destfile = paste0("data/mata/", "refresh-", make.names(Sys.time()), "-", tmp, ".json")),
         error = function(e) print(paste("refresh", 'did not work out')))
tryCatch(download.file(paste0("https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/iv/message?_tmp=", tmp), destfile = paste0("data/mata/", "message-", make.names(Sys.time()), "-", tmp, ".json")),
         error = function(e) print(paste("messages", 'did not work out')))

#i need to do something different with this, it'll bog everything down
# if(minute(Sys.time()) > 30 & minute(Sys.time()) < 45 & hour(Sys.time())) {
#   for(i in seq_along(stop.id)) {
#     tryCatch(
#       download.file(paste0("https://mata.cadavl.com:4437/SWIV/MATA/proxy/restWS/horaires/pta/", stop.id[i], "?_tmp=", tmp), destfile = paste0("data/mata/", "pta-horaire-", stop.id[i], "-", make.names(Sys.time()), "-", tmp, ".json")),
#       error = function(e) print(paste("stop.id", stop.id[i], "pta", 'did not work out'))
#     )
#
#   }
# }

