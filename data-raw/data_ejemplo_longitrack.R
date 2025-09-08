# data-raw/make_data_from_excel.R
# Lee dummies desde DEV/*.xlsx (solo local) y crea data/pre,long,post
library(readxl)

pre  <- read_excel("dev/dummy_data.xlsx", sheet = "pre")
long <- read_excel("dev/dummy_data.xlsx", sheet = "long")
post <- read_excel("dev/dummy_data.xlsx", sheet = "post")

# --- guardar en data/ ------------------- 
usethis::use_data(pre, long, post, overwrite = TRUE, compress = "xz")
