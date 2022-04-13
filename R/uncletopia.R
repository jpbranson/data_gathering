tryCatch(
  download.file("https://uncletopia.com/api/servers", destfile = paste0("data/uncletopia/", make.names(Sys.time()), ".json")),
  error = function(e) print(paste("servers", 'did not work out'))
  )


