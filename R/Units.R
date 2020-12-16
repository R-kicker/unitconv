# 26-04-2016
# Experimental module for units of physical variables

#==================================#
# Section 0: Constants          ####
#==================================#
#' @title Built-in constant: Air molecular mass
#' @description Air molecular mass, 28.967 [g/mole]
#' @export
MWair <- 28.967        # g/mole, (C. Whitson, Ch.3, p.22)

#' @title Built-in constant: Universal gas constant
#' @description Universal gas constant in SI units, 8.3144621 [J/(mole*K)]
#' @export
R_SI <- 8.3144621      # Joule / (mole * Kelvin)

#' @title Built-in constant: Universal gas constant
#' @description Universal gas constant in American FIELD units, 10.7315 [psi*ft3/(lb-mole*R)]
#' @export
R_US <- 10.7315        # psia * ft^3 / (lb-mole * Rankine)

#' @title Built-in constant: Rankine to Farenheit temperature
#' @description Rankine to Farenheit temperature constant: 459.67 degrees
#' @export
TF_TR <- 459.67        # degree Rankine to Farenheit

#' @title Built-in constant: Celsius to Kelvin temperature
#' @description Celsius to Kelvin temperature constant: 273.15 degrees
#' @export
TC_TK <- 273.15        # degree Kelvin to Celsius

#' @title Built-in constant: standard pressure
#' @description Standard pressure is METRIC units, 1.01325 [bar]
#' @export
Psc_SI <- 1.01325      # Standard pressure, metric units

#' @title Built-in constant: standard temperature
#' @description Standard temperature is METRIC units, 293.15 [K]
#' @export
Tsc_SI <- 20 + TC_TK  # Standard temperature, metric units

#' @title Built-in constant: standard pressure
#' @description Standard pressure is FIELD units, 14.696 [psia]
#' @export
Psc_US <- 14.696       # Standard pressure, US oilfield units

#' @title Built-in constant: standard temperature
#' @description Standard temperature is FIELD units, 519.67 [R]
#' @export
Tsc_US <- 60 + TF_TR  # Standard temperature, US oilfield units

#' @title Built-in constant: standard water density
#' @description Standard water density at FIELD standard conditions, 62.3664 [lb/ft3]
#' @export
wd_US <- 62.3664       # 14.696 psia, 60 F

#' @title Built-in constant: standard air density
#' @description Standard air density at FIELD standard conditions, 0.076362 [lb/ft3]
#' @export
ad_US <- 0.076362      # 14.696 psia, 60 F

#' @title Built-in constant: standard air density
#' @description Standard air density at METRIC standard conditions, 1.2232 [kg/m3]
#' @export
ad_SI <- 1.2232        # 1 atm, 20 C

#' @title Molar volume in METRIC units
#' @description Molar volume in METRIC units [l] per 1 g-mole
#'  as per Avogadro law - i.e. T=0 C and p=1 atm = 1.01325 bar
#' @examples
#'  Vm_Avo()
#' @export
Vm_Avo <- function()
  return(22.413962)    # 1 atm, 0 C

#' @title Molar volume in METRIC units
#' @description Molar volume in METRIC units [l] per 1 g-mole
#'  at FIELD standard conditions(!) - i.e. T=60 F and p=14.696 psia
#' @examples
#'  library(unitconv)
#'  Vm_SI()
#' @export
Vm_SI <- function()
  return(23.70336)     # 14.696 psia, 60 F

#' @title Molar volume in FIELD units
#' @description Molar volume in FIELD units [scf] per 1 lb-mole
#' @examples
#'  library(unitconv)
#'  Vm_US()
#' @export
Vm_US <- function()
  u(uc(Vm_SI(), "l", "ft3") * uc(1, "lb", "g"), "scf")

#==================================#
# Section 1: Conversion factors ####
#==================================#

#' @title Important conversion factors apart from unit lists
#' @description Mass: pound [lb] to [kg] conversion factor
#' @export
lb_kg <- 0.4535924

#' @title Important conversion factors apart from unit lists
#' @description Length: foot [ft] to [m] conversion factor
#' @export
ft_m <- 0.3048

#' @title Important conversion factors apart from unit lists
#' @description Volume: barrel [bbl] to cubic feet [ft3] conversion factor
#' @export
bbl_ft3 <- 1 / 0.1781076

#' @title Important conversion factors apart from unit lists
#' @description Area: millidarcy [mD] to square meter [m2] conversion factor
#' @export
mD_m2 <- 0.9869e-15

#' @title Important conversion factors apart from unit lists
#' @description Pressure: psi [psi]=[lb/in2] to Pascal [Pa] conversion factor
#' @export
psi_Pa <- 6894.757

#' @title Important conversion factors apart from unit lists
#' @description Pressure: atmposphere [atm] to Pascal [Pa] conversion factor
#' @export
atm_Pa <- 101325.3

#' @title Important conversion factors apart from unit lists
#' @description Pressure: atmposphere [atm] to absolute atmposphere [ata] conversion factor
#' @export
atm_ata <- 1.033214

#' @title Important conversion factors apart from unit lists
#' @description Pressure: atmposphere [atm] to Hg millimeters [mmHg] conversion factor
#' @export
atm_mmHg <- 760.0008

#' @title Important conversion factors apart from unit lists
#' @description Energy: British thermal unit [BTU] to Joule/mole [J/mole] conversion factor
#' @export
BTU_Jmol <- 2236

#' @title Important conversion factors apart from unit lists
#' @description Time: day to second conversion
#' @export
day_s <- 24*60*60

#==================================#
# Section 2: Unit lists         ####
#==================================#
#' @title Mass unit list
#' @description Mass unit list
#' @details Valid units: "mkg", "mg", "g", "kg", "ton", "lb" (pound) and "ou" (ounce)
#' @export
Mass <- function()
  list(
    units = c("mkg", "mg", "g", "kg", "ton", "lb", "ou"),
    const = c(1e9, 1e6, 1e3, 1, 1e-3, 1/lb_kg, 16/lb_kg)
  )

#' @title Length unit list
#' @description Length unit list
#' @details Valid units: "nm", "mkm", "mm", "cm", "dm", "m", "km", "in" and "ft"
#' @export
Length <- function()
  list(
    units = c("nm", "mkm", "mm", "cm", "dm", "m", "km", "in", "ft"),
    const = c(1e9, 1e6, 1e3, 1e2, 1e1, 1, 1e-3, 12/ft_m, 1/ft_m)
  )

#' @title Area unit list
#' @description Area unit list
#' @details Valid units: "mkm2", "mm2", "cm2", "dm2", "m2", "km2", "ha",
#'  "mD", "D", "in2" and "acre"
#' @export
Area <- function()
  list(
    units = c("mkm2", "mm2", "cm2", "dm2", "m2", "km2", "ha",
              "mD", "D", "in2", "acre"),
    const = c(1e12, 1e6, 1e4, 1e2, 1, 1e-6, 1e-4,
              1/(mD_m2*1e-3), 1/mD_m2, (12/ft_m)^2, 1/4046.86)
  )

#' @title Volume unit list
#' @description Volume unit list
#' @details Valid units: "cm3", "l", "gal", "m3", "bbl" (barrel) and "ft3"
#' @export
Volume <- function()
  list(
    units = c("cm3", "l", "gal", "m3", "bbl", "ft3"),
    const = c(1e6, 1e3, 3.785411784e3, 1, 1/0.1589873, (1/ft_m)^3)
  )

#' @title Pressure unit list
#' @description Pressure unit list
#' @details Valid units: "mPa", "Pa", "kPa", "MPa", "GPa",
#'  "psi", "atm", "ata" (absolute atmosphere), "bar" and "mmHg"
#' @export
Pressure <- function()
  list(
    units = c("mPa", "Pa", "kPa", "MPa", "GPa",
              "psi", "atm", "ata", "bar", "mmHg"),
    const = c(1e3, 1, 1e-3, 1e-6, 1e-9,
              1/psi_Pa, 1/atm_Pa, atm_ata/atm_Pa, 1e-5, atm_mmHg/atm_Pa)
  )

#' @title Density unit list
#' @description Density unit list
#' @details Valid units: "g/cm3", "kg/m3" and "lb/ft3"
#' @export
Density <- function()
  list(
    units = c("g/cm3", "kg/m3", "lb/ft3"),
    const = c(1e-3, 1, 1/lb_kg/((1/ft_m)^3))
  )

#' @title Dynamic Viscosity unit list
#' @description Dynamic Viscosity unit list
#' @details Valid units: "mkP", "cP", "P and "Pa*s"
#' @export
DynViscosity <- function()
  list(
    units = c("mkP", "cP", "P", "Pa*s"),
    const = c(1e4, 1, 1e-2, 1e-3)
  )

#' @title Whole unit list of functions
#' @description Whole unit list of functions
#' @export
measurelist <- function()
  c(Mass = Mass, Length = Length, Area = Area, Volume = Volume,
    Pressure = Pressure, Density = Density, DynVisc = DynViscosity)

#==================================#
# Section 3: Conversion functions####
#==================================#
#' @title Unit conversion function
#' @description Unit conversion function "from"-"to"
#' @references
#'  \enumerate{
#'   \item \url{http://en.wikipedia.org/wiki/Systems_of_measurement}
#'   \item \url{http://en.wikipedia.org/wiki/Conversion_of_units}
#'   \item \url{http://en.wikipedia.org/wiki/Api_gravity}
#'   \item \url{http://cran.r-project.org/web/packages/marelac/}
#'   \item \url{http://petrowiki.org}
#'  }
#' @param x A quantity to be conversed (atomic vector or list)
#' @param from Valid unit name (character)
#' @param to Valid unit name (character)
#' @return Converted quantity (atomic vector or list as per input)
#' @examples
#'  library(unitconv)
#'  uc(1, "mkm2", "D")
#'  uc(1, "mkm2", "mD")
#'  uc(u(1:5, "kg"), "kg", "lb")
#'  uc(u(list(1, 2), "ton"), "ton", "lb")
#'  uc(u(data.frame(x1 = 1:3, x2 = 4:6), "ft3"), "ft3", "cm3")
#'  uc(1, "ft3", "l")
#'  uc(1, "ft3", "cm3")
#'  uc(1, "bbl", "l")
#'  uc(150, "m", "km") + u(c(1.0, 2.0, 3.5), "km")
#'  uc(1.5, "cP", "Pa*s")
#'  # Example below with correct units
#'  uc(1, "kg/m3", "lb/ft3")
#'  # Example below with incorrect units causes an error
#'  \dontrun{uc(1, "kg/m3", "lbft3")}
#' @export
uc <- function(x, from, to) {
  validunits <-
    unlist(sapply(X = measurelist(), FUN = function(f) f()$units))
  if (!((from %in% validunits) & (to %in% validunits)))
    stop("Invalid units input")
  lf <- sapply(X = measurelist(),
               FUN = function(f, x) if (x %in% f()$units) f else NULL,
               x = from)
  phys <- lf[[which(sapply(X = lf, FUN = function(f) is.null(f)) == FALSE)]]
  i <- which(phys()$units == from)
  j <- which(phys()$units == to)
  m <- phys()$const[j] / phys()$const[i]
  # which **ply function we shall use ??
  if (is.data.frame(x)) {
    # is this OK?
    z <- as.data.frame(sapply(x, `*`, m))
  } else if (is.list(x)) {
    z <- lapply(x, `*`, m)
  } else {
    z <- sapply(x, `*`, m)
  }
  if (is.physical(x)) z <- u(z, to)
  return(z)
}

#' @title Unit create function
#' @description Creates a class with specified unit
#' @param x A quantity to use as physical meaning value
#' @param unit Valid unit name
#' @return Object of class "physical"
#' @examples
#'  library(unitconv)
#'  z1 <- u(1:5, "km")
#'  print(z1)
#'  z2 <- u(list(1, 2), "kg")
#'  print(z2)
#' @export
u <- function(x, unit) {
  attr(x, "unit") <- unit
  # !!! Need to preserve existing S3 class attribute
  # !!! The order of class names is important: new class before old class
  structure(x, class = c("physical", class(x)))
}

#' @title Unit destroy function
#' @description Removes units from vector of list
#' @param x A quantity of class "physical"
#' @return Dimensionless vector or list
#' @examples
#'  library(unitconv)
#'  z1 <- u(list(1, 2, 3), "km")
#'  print(z1)
#'  z2 <- un(z1)
#'  print(z2)
#' @export
un <- function(x) {
  attr(x, "unit") <- NULL
  # unclass(x) - this is weird!!! destroys all classes
  # need to preserve existing S3 classes
  cl <- class(x)
  class(x) <- cl[setdiff(seq_along(cl), which(cl == "physical"))]
  return(x)
}

#' @title Temperature conversion
#' @description Temperature conversion: Celsius to Farenheit
#' @param x A quantity to convert from, [C]
#' @return Converted quantity, [F]
#' @examples
#'  library(unitconv)
#'  C_F(15.6)
#' @export
C_F <- function(x) {
  u(x * 9 / 5 + 32, "deg.F")
}

#' @title Temperature conversion
#' @description Temperature conversion: Celsius to Rankine
#' @param x A quantity to convert from, [C]
#' @return Converted quantity, [R]
#' @examples
#'  library(unitconv)
#'  C_R(15.6)
#' @export
C_R <- function(x) {
  u(1.8 * (x + TC_TK), "deg.R")
}

#' @title Temperature conversion
#' @description Temperature conversion: Farenheit to Celsius
#' @param x A quantity to convert from, [F]
#' @return Converted quantity, [C]
#' @examples
#'  library(unitconv)
#'  F_C(60)
#' @export
F_C <- function(x) {
  u((x - 32) * 5 / 9, "deg.C")
}

#' @title Temperature conversion
#' @description Temperature conversion: Rankine to Celsius
#' @param x A quantity to convert from, [F]
#' @return Converted quantity, [C]
#' @examples
#'  library(unitconv)
#'  R_C(60 + TF_TR)
#' @export
R_C <- function(x) {
  u(x / 1.8 - TC_TK, "deg.C")
}

#' @title Density - Gravity conversion
#' @description Density conversion: METRIC [kg/m3] to FIELD [API]
#' @param x A quantity to convert from, [kg/m3]
#' @return Converted quantity, [API]
#' @examples
#'  library(unitconv)
#'  kgm3_API(865)
#' @export
kgm3_API <- function(x) {
  u(gcc_API(x / 1000), "API")
}

#' @title Density - Gravity conversion
#' @description Density conversion: FIELD [API] to METRIC [kg/m3]
#' @param x A quantity to convert from, [API]
#' @return Converted quantity, [kg/m3]
#' @examples
#'  library(unitconv)
#'  API_kgm3(32)
#' @export
API_kgm3 <- function(x) {
  uc(API_gcc(x), "g/cm3", "kg/m3")
}

#' @title Density - Gravity conversion
#' @description Density conversion: METRIC [g/cm3] (specific gravity) to FIELD [API]
#' @param x A quantity to convert from, [g/cm3]
#' @return Converted quantity, [API]
#' @examples
#'  library(unitconv)
#'  gcc_API(0.865)
#' @export
gcc_API <- function(x) {
  u(141.5 / x - 131.5, "API")
}

#' @title Density - Gravity conversion
#' @description Density conversion: FIELD [API] to METRIC [g/cm3] (specific gravity)
#' @param x A quantity to convert from, [API]
#' @return Converted quantity, [g/cm3]
#' @examples
#'  library(unitconv)
#'  API_gcc(32)
#' @export
API_gcc <- function(x) {
  u(141.5 / (131.5 + x), "g/cm3")
}

#' @title Pressure conversion
#' @description Pressure conversion: FIELD [psi] to METRIC [bar]
#' @param x A quantity to convert from, [psi]
#' @return Converted quantity, [bar]
#' @examples
#'  library(unitconv)
#'  psi_bar(32)
#' @export
psi_bar <- function(x) {
  uc(x, "psi", "bar")
}

#' @title Pressure conversion
#' @description Pressure conversion: METRIC [bar] to FIELD [psi]
#' @param x A quantity to convert from, [bar]
#' @return Converted quantity, [psi]
#' @examples
#'  library(unitconv)
#'  bar_psi(32)
#' @export
bar_psi <- function(x) {
  uc(x, "bar", "psi")
}

#' @title Pressure conversion
#' @description Pressure conversion: FIELD [psi] to METRIC [MPa]
#' @param x A quantity to convert from, [psi]
#' @return Converted quantity, [MPa]
#' @examples
#'  library(unitconv)
#'  psi_MPa(32)
#' @export
psi_MPa <- function(x) {
  uc(x, "psi", "MPa")
}

#' @title Pressure conversion
#' @description Pressure conversion: METRIC [MPa] to FIELD [psi]
#' @param x A quantity to convert from, [MPa]
#' @return Converted quantity, [psi]
#' @examples
#'  library(unitconv)
#'  MPa_psi(32)
#' @export
MPa_psi <- function(x) {
  uc(x, "MPa", "psi")
}

#' @title Gas-liquid ratio conversion
#' @description Gas-liquid ratio conversion: METRIC [m3/m3] to FIELD [scf/STB]
#' @param x A quantity to convert from (numeric vector), [m3/m3]
#' @return Converted quantity (numeric vector), [scf/STB]
#' @examples
#'  library(unitconv)
#'  GLR_US(c(85, 110))
#' @export
GLR_US <- function(x) {
  x * u(uc(1, "m3", "ft3") / uc(1, "m3", "bbl"), "scf/STB")
}

#' @title Gas-liquid ratio conversion
#' @description Gas-liquid ratio conversion: FIELD [scf/STB] to METRIC [m3/m3]
#' @param x A quantity to convert from (numeric vector), [scf/STB]
#' @return Converted quantity (numeric vector), [m3/m3]
#' @examples
#'  library(unitconv)
#'  GLR_SI(c(500, 1100))
#' @export
GLR_SI <- function(x) {
  x * u(1 / GLR_US(1), "m3/m3")
}

#==================================#
# Section 4: Generic functions  ####
#==================================#
#' @title Generic function: check physical measure
#' @description Generic function: check physical measure
#' @param x A quantity to check
#' @return TRUE or FALSE whether x is an object of class "physical"
#' @examples
#'  library(unitconv)
#'  is.physical(32)
#'  is.physical(u(32, "deg.C"))
#' @export
is.physical <- function(x) {
  return(any(class(x) == "physical"))
}

#' @title Generic function: print physical measure
#' @description Generic function: print physical measure
#' @param x A quantity to print
#' @param ... Optional arguments to be passed further
#' @return Prints to console x with "Units: " at the beginning of the object output
#' @examples
#'  library(unitconv)
#'  z <- round(runif(10)*100, 1)
#'  print(u(z, "mD"))
#' @export
print.physical <- function(x, ...) {
  uattr <- attr(x, "unit")
  if (is.null(uattr)) {
    uattr <- "N/A"
  } else {
    attr(x, "unit") <- NULL
  }
  print(paste0("Units: ", uattr))
  # don't use unclass! - a weird way
  print(un(x), ...)
}

#==================================#
# Section 5: EXAMPLES           ####
#==================================#
# !!! moved to "testthat" directory !!!
