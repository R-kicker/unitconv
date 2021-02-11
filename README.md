# unitconv
Unit conversion routines in Petroleum Engineering

Unit conversion features for physical measures implemented
 for SI METRIC and American FIELD units of Petroleum Engineering:
 - special class "physical" with attribute "unit";
 - fixed constants (universal gas constant, molar volume etc.);
 - unit conversion functions (specific to industry).

This R-script has documentation as per roxygen2 standard and converted to R-package.

Please note there is an existing R-package called `units`: https://cran.r-project.org/web/packages/units/index.html
<P>
So avoid confliting names this package called `unitconv`.
Although, the idea was quite similar to attach an attribute "unit" to any numeric quantity in R.
