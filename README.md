# NMR.Utils

Contains functions to automatically import, process and plot Bruker NMR data.
Primarily designed to speed up the processing of in-situ electrochemical experiments.

## Installation

1. Install the devtools package if you do not already have it:

   `install.packages("devtools")`

2. Install this package

   `devtools::install_github("jmstrat/NMR.Utils")`

## Details

### Graphical interface for in situ data processing

This package includes a graphical interface for importing, processing, and plotting *in situ* / *operando* NMR data.
This processing wizard can be summoned using the incantation `insitu_gui()`.

### Importing NMR data

Use the function `read.nmr` to import 1D or 2D nmr data. Only processed bruker data (1D / 2D binary files),
or files exported from topspin (`convbin2asc` / `totxt`) are supported.

Use `plot` or `iplot` to plot the data.

### Scripting interface for in situ NMR data

* Import the NMR data using `read.nmr`; import the echem data using `read.echem` from the
  [Echem.Data](https://github.com/jmstrat/Echem.Data/) package.

* Set the time for each scan using `storeOffsets`.
  These times can be automatically generated from an ATMC log file using `read.ATMC`
  or without a log file using `noATMoffsets`.

* Combine the NMR and echem data with `associate_echem_with_nmr`.

* If complex, data can then be phased (`apkpseudo2d`), and made real (`makeReal`).

* Baseline subtraction can be performed using `make_background` OR `correct_baseline`

* Use `plot` to plot the data – see `?plot.nmr2dinsitu.data.object` for options.

Any / all of these steps can be performed graphically using the command `insitu_gui`
or any of the interactive family of commands (`interactiveImport`, `interactivePhase`,
`interactiveBaseline` and `interactivePlotting`). Data can be exported as a csv file
using the `export` function.

### Fitting in situ NMR data

Import and process the data as described above. Note that it is very important that the
baseline is 0 for fitting, if your baseline is reasonably flat the command
`data = correct_baseline(data, method="modpolyfit",degree=1)` will fit a straight line
to the data. Otherwise consider using `interactiveBaseline`.

* Create a fit object using `new_fit`, e.g.
  `fit = new_fit(integration_range=c(-50, 50))`

* Create one or more peaks using `new_model`, e.g.:
  `electrolyte = new_model(pseudoVoigt, height=1e6, centre=0.2, hwhm=2.0, shape=0.5)`

* Define which parameters of the model will be fitted using `add_constraint_to_model`, e.g.:
  `electrolyte = add_constraint_to_model(electrolyte, parameter='height', constraint_type='vary', constraint=1e5)`

* Add each model to the fit using `add_model_to_fit`, e.g.:
  `fit = add_model_to_fit(fit, electrolyte)`

* Run the fit using `run_fit_for_data`.

Results are available in the fit object under `result`, e.g. `fit$result`.
These can be exported to a csv file using `export`. A simple plot can be made using `plot(fit)`,
or an animation as described below.

### Animations

A simple animation of in situ NMR data can be made using `save_animation`, optionally including a fit.

## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details
