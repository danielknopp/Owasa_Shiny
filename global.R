library(magrittr)

dir   <- "R"
files <- list.files(dir)

for(i in 1:length(files))
	source(file.path(dir, files[i]))
