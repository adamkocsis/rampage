if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/rampage")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/rampage")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/rampage")
