# FSAWS 0.0.3
* Added `graphics`, `grDevices`, and `stats` to, and removed `gplots` (see `paletteChoices()` below) from, IMPORTS.
* Added `FSA` to IMPORTS (see `lencatoLD()` below).
* Added `testthat` to SUGGESTS.
* Added tests for `rlp()`, `emp()`, and `wsValidate()`.

* `.onAttach()`: Streamlined output.
* `emp()`: Needed to add `as.numeric()` around `p.table.n` when making the regression data.frame so that it contained only the sample sizes (and not the labels).
* `lencatOLD()`: Removed internal function, replaced with `FSA::lencat()`. This required changes to `emp()` and `wsValidate()` (*tests held after changes*).
* `palletteChoices()`: Removed `rich` as an option so that `gplots` could be removed from IMPORTS.
* `wsValidate()`: Needed to add `as.numeric()` around `p.table.n` when making the regression data.frame so that it contained only the sample sizes (and not the labels).

# FSAWS 0.0.2 6-Jan-2023 
* DESCRIPTION file: Added `LazyData: true` and `Encoding: UTF-8` to, fixed title and author fields.
# Removed `FSA` (not needed when package moved) and `gdata` (replaced all `drop.levels()` with `droplevels()`) from "Imports"
* Handled issues with "@S3Method" in `emp()`, `FroeseWs`, `rlp()`, `wsValidate()`.
* Fixed line lengths and added `package::` where needed in `emp()`, `FroeseWs`, `rlp()`, `wsValidate()`, and `chooseColors()`.

* `chooseColors()` and `paletteChoices()`: Removed `jet` option (so as not to require a package dependency).
* `emp()`: Removed related `residPlot()` (use `plot()` instead as shown in some examples).
* `fitPlot()`: Added generic function so that `fitPlot.emplm()`, `fitPlot.empq()`, `fitPlot.emprq()`, and `fitPlot.rlp()` would all work without depending on another package (i.e., `FSAmisc`).
* `residPlot()`: Removed (all examples show how to form with `plot()`).
* `rlp()`: Removed related `residPlot()` (use `plot()` instead as shown in some examples).

# FSAWs 0.0.1 2014
* Started this package with functions from FSA v0.4.6 and FSAdata v0.1.5.
* `BluegillWs`: added.
* `emp()`: added.
* `FroeseWs()`: added.
* `lencatOLD()`: added as an internal function.
* `LMBassLWs`: added.
* `rlp()`: added.
* `RuffeWs`: added.
* `WalleyeGerowLW`: added.
* `WalleyeWs`: added.
* `wsValidate()`: added.
