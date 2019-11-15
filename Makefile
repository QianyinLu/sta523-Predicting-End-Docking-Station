hw6.html: hw6.Rmd weather.rds
	Rscript -e "library(rmarkdown); render('hw6.Rmd')"