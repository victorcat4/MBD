.onAttach <- function(libname, pkgname) {
  packageStartupMessage("MBD: Download and process Movebank data")
  packageStartupMessage("Use run_pipeline('config.yaml') to get started")
}
