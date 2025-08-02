load_packages <- function(package_list) {
  # Extract package names from parentheses if present
  packages <- gsub("^library\\(|\\)$", "", package_list)
  
  # For each package
  lapply(packages, function(pkg) {
    # Check if package is installed, if not install it
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      # Try loading again after installation
      if (!require(pkg, character.only = TRUE)) {
        warning(paste("Package", pkg, "couldn't be installed/loaded"))
      }
    }
  })
}
