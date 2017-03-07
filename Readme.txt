MOSfieldutils -- some notes about making R package

How to use the NAMESPACE file?
The code uses I use roxygen2, see http://r-pkgs.had.co.nz/namespace.html

"If you are using just a few functions from another package, my recommendation is to note the package name in the Imports: field of the DESCRIPTION file and call the function(s) explicitly using ::, e.g., pkg::fun()."

How to set global options?
Now some initial ideas in MOSsetup.R

