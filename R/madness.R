
if(as.Date(Sys.time()) <= as.Date("2022-04-04")) {
  download.file("https://dl.dropboxusercontent.com/s/j12vjbcpay9haeh/madness.html?dl=1", destfile = paste0("data/madness/", make.names(Sys.time()), ".html"))
}

