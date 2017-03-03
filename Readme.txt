MOSfieldutils -- some notes

How to use the NAMESPACE file?
Should I use roxygen2 (what is this) http://r-pkgs.had.co.nz/namespace.html#search-path
#' @export

Do I need imports in NAMESPACE, yes I do.

"If you are using just a few functions from another package, my recommendation is to note the package name in the Imports: field of the DESCRIPTION file and call the function(s) explicitly using ::, e.g., pkg::fun()."

How to set global options?


