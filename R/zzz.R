.models <- new.env()

.onLoad <- function(libname, pkgname) {
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 3)
  .models$model_list <- list()
}
