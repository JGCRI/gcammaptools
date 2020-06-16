[![Build Status](https://travis-ci.org/JGCRI/gcammaptools.svg?branch=master)](https://travis-ci.org/JGCRI/gcammaptools)

# gcammaptools
gcammaptools is an R package designed to deliver dynamically generated, highly customizable mapping products

## Overview

The purpose of the `gcammaptools` package is to provide standardized functions that can handle a variety of different shape and data field arguments to dynamically generate world or regional based data driven maps. This includes functions for making plots based on regional (choropleth) or gridded (downscaled) data, as well as default projection and theme settings that provide a house style for plot aesthetic standardization. The package also provides the ability to plot data from our [GCAM](http://jgcri.github.io/gcam-doc/) system to GCAM based regions.

## Getting Setup with the `gcammaptools` Package

### Installation

This package must be installed from the github repository located [here](https://github.com/JGCRI/gcammaptools) using the `install_github` command from the `devtools` package. To do this, you will need to install the `devtools` package first if you do not have it already. Additionaly, this package also depends on the following R packages:

* RColorBrewer
* [rgis](https://github.com/JGCRI/rgis)
* classInt
* dplyr
* ggplot2 (currently supports 3.2 or lower)
* ggspatial
* magrittr
* raster
* sf

### Step 1: Install package
```R
install.packages('devtools') # if you don't have it already
devtools::install_github('JGCRI/gcammaptools')
```  
### Step 2: Build vignettes
Optionally, you can build the vignettes including a tutorial for how to use the package along with some examples, which shows how to do several common mapping tasks.  To build these examples, replace the `install_github` command above with:
```R
devtools::install_github('JGCRI/gcammaptools', dependencies=TRUE,
                         build_vignettes=TRUE)
```
You can display the choropleth vignette by running
```R
vignette('choropleth','gcammaptools')
```

Sometimes R can't find the CRAN repository without help.  If during the `install_github` steps you get errors about missing packages or functions, rerun them with the `repos` option:  

```R
cran <- 'http://cran.us.r-project.org'
devtools::install_github('JGCRI/gcammaptools', build_vignettes=TRUE,
                         dependencies=TRUE, repos=cran)
```  
This should allow R to fetch the packages it needs to complete the installation from CRAN.

## Usage

### Preparing GCAM data

The recommended way to load your GCAM data is by using the 
[rgcam](https://github.com/JGCRI/rgcam) package create a project data file from
a GCAM database, and then querying that file for the data you want to plot. 
```R
prj <- rgcam::loadProject('myproj.dat')
co2 <- rgcam::getQuery(prj, 'CO2 emissions by region', 'Reference')
```
Alternatively, you can start with any data frame that has a `region` column and
one or more data columns.

Once you have loaded the data, you must add the region identifiers used in the
map data to the data frame using the `add_region_ID` function:
```R
co2 <- add_region_ID(co2)
```

### Plotting maps

The main plotting function is `plot_GCAM`.  You can supply your own map of
region boundaries, but most of the time you will want to use the ones supplied
with the package.
`plot_GCAM`:  
```R
plot_GCAM(map.rgn32, col='value', gcam_df=co2, title='CO2 Emissions', legend=TRUE)
```

[![DOI](https://zenodo.org/badge/254437500.svg)](https://zenodo.org/badge/latestdoi/254437500)
 [![Build Status](https://travis-ci.org/IMMM-SFA/im3py.svg?branch=master)](https://travis-ci.org/IMMM-SFA/im3py) [![codecov](https://codecov.io/gh/IMMM-SFA/im3py/branch/master/graph/badge.svg)](https://codecov.io/gh/IMMM-SFA/im3py)

# im3py
An IM3 template repository for Python projects that have time-steps


## Overview
The purpose of `im3py` is to help developers quickly establish a GitHub repository that conforms to IM3 software engineering standards.  Our hope is to create a common user experience for all Python modeling software developed for use in IM3 experiments.  We are mindfully developing software that exposes key variables per time-step so that they may be used in integrated and/or uncertainty characterization experiments while still maintaining the ability for autonomous use.  This template package establishes the structure necessary to wrap existing Python code in our modeling interface and time-step processing generator.  We also include:  a sample test suite, `Zenodo`, `Travis-CI`, and `codecov` standard files and setup protocol, our expected `docstring` style, a `stdout` and file logger, and an example fake code file that represents a user's code contribution.

## Getting Setup with the `im3py` Template Repository

### Using the Template
Simply click `Use this template` on the main repository page (shows up to the left of `Clone or download`) and fill in your `Repository name`, the `Description`, select whether you want the repository to be `Public` or `Private`, and leave `Include all branches` unchecked.  Repository names for Python packages should match the name of the actual package if at all possible.  Python package name conventions should be all lower case and only separated by an underscore if necessary.  We highly encourage `Public` development as well.

### Setting up Travis-CI, Codecov, and Zenodo for your New Repository
We use Travis-CI for continuous integration testing to ensure we do not make changes to our code that cause our tests to fail.  The sample `.travis.yml` file is currently setup to test on Windows, Mac, and Linux. You may need to tailor this script to install other libraries before your package is installed on a Travis-CI virtual environment (e.g., GDAL).  Here is some Travis-CI info if you want to learn more:  [Core Concepts for Beginners](https://docs.travis-ci.com/user/for-beginners/).

We also use Codecov as a way to measure how well we are covering our code with tests.  Codecov fits nicely within our Travis-CI setup.  Please contact our software engineering team for a demo of how to get the most from the information this software provides.  Here is some Codecov info if you wish to learn more:  [About Code Coverage](https://docs.codecov.io/docs/about-code-coverage).

As we develop our software, we want to make sure that we conduct relevant and timely releases that are linked to a permanent archive and have a resulting DOI.  We do this in IM3 by linking Zenodo to our repositories.  When we conduct a GitHub release, the resulting archive is automatically created in Zenodo.  This can then be sited in a journal article or meta-repository.  Here is some Zenodo info if you wish to learn more:  [About Zeonodo](https://about.zenodo.org/).

IM3 already has Zenodo, Travis-CI, and Codecov accounts setup, so when you create a repository from this template please notify our software engineering team and we will "flip the switch" to make our accounts recognize your new repository.

You will also need to update the links in the badges at the top of this document to point to your model's information.

## Understanding the `im3py` repository
The following table outlines each part of the repository and it's purpose:

| Component | Description |
| ---- | ---- |
| `.gitignore` | Which files Git will ignore in your local when you push your changes to the remote |
| `.travis.yml`| Setup YAML file for Travis-CI; includes instantiation of Codecov
| `LICENSE` | The appropriate open-source license.  Work with the software engineering team to ensure this is correct for your software |
| `MANIFEST.in` | A log of files to include in your source distribution that are not automatically included by default |
| `README.md` | A README file in Markdown that will display on the splash page of the repository |
| `requirements.txt` | A text file of required non-built-in Python packages to install |
| `setup.py` | A python file that is equipped with information to install the Python code as a package |
| `im3py` | The directory containing the Python package code |
| `im3py/__init__.py` | Allows Python to recognize a directory as a package.  This one raises classes and functions to be accessible to the user from the package level |
| `im3py/model.py` | A model class that instantiates a logger and runs the model under user defined conditions |
| `im3py/process_step.py` | A class that the generator is built from which allows the user to place conditions on how the model will run per time-step |
| `im3py/read_config.py` | A class that reads the configuration file or from arguments passed into the model class |
| `im3py/install_supplement.py` | A class that downloads and unpacks an example data supplement from a remote source that matches the current installed distribution |
| `im3py/some_code.py` | Fake code to represent what a user may provide.  This file should be removed. |
| `im3py/tests` | The module holding the test suite |
| `im3py/tests/test_model.py` | Tests for model.py |
| `im3py/tests/test_process_step.py` | Tests for process_step.py |
| `im3py/tests/test_read_config.py` | Tests for read_config.py |
| `im3py/tests/test_install_supplement.py` | Tests for install_supplement.py |
| `im3py/tests/test_some_code.py` | Tests for some_code.py |
| `im3py/tests/data` | Directory holding test data.  Optional directories are `inputs` and `comp_data`.  The `outputs` are not housed in the repository. |
| `im3py/tests/data/inputs` | Directory housing inputs that should be expected for a subset of a run |
| `im3py/tests/data/inputs/config.yml` | Sample configuration YAML file used in tests |
| `im3py/tests/data/comp_data` | Directory housing outputs that should be expected for a subset of a run.  Test sets should be subsets and be able to run quickly where possible. |
| `im3py/tests/data/comp_data/output_year_2015.txt` | Expected output file for time-step 2015 |
| `im3py/tests/data/comp_data/output_year_2016.txt` | Expected output file for time-step 2016 |
| `im3py/tests/data/comp_data/test_no-header.csv` | Expected data from install supplement download |


## Getting Started Using the `im3py` Package
The `im3py` package uses only **Python 3.6** and up.

### Step 1:
You can install `im3py` by running the following from your cloned directory (NOTE: ensure that you are using the desired `pip` instance that matches your Python3 distribution):

`pip3 install git+https://github.com/IMMM-SFA/im3py.git --user`

### Step 2:
Confirm that the module and its dependencies have been installed by running from your prompt:

```python
from im3py import Model
```

If no error is returned then you are ready to go!

## Setting up a run

### Expected arguments
See examples below for how to pass into the `Model` class

| Argument | Type | Description |
|----|----|----|
| `config_file` | str | Full path to configuration YAML file with file name and extension. If not provided by the user, the code will default to the expectation of alternate arguments. |
| `output_directory` | string | Full path with file name and extension to the output directory where outputs and the log file will be written. |
| `start_step` | int | Start time step value. |
| `through_step` | int | Through time step value. |
| `time_step` | int | Number of steps (e.g. number of years or minutes between projections) |
| `alpha_param` | float | Alpha parameter for model.  Acceptable range:  -2.0 to 2.0 |
| `beta_param` | float | Beta parameter for model.  Acceptable range:  -2.0 to 2.0 |
| `write_logfile` | bool | Optional, choose to write log as file. |

### Variable arguments
Users can update variable argument values after model initialization; this includes updating values between time steps (see **Example 3**).  The following are variable arguments:
- `alpha_param`
- `beta_param`

### YAML configuration file option (e.g., config.yml)
Arguments can be passed into the `Model` class using a YAML configuration file as well (see **Example 1**):

```yaml
# Example configuration file setup
output_directory:  "<Full path to the output directory>"
start_step: 2015
through_step: 2016
time_step: 1
alpha_param: 2.0
beta_param: 1.42
write_logfile: False
```

### Expected outputs
Each time-step processed will generate a TEXT file containing a solution message and have the file name formatted as `output_year_<YYYY>.txt`. These will be written to where the `output_directory` has been assigned.

## Examples

### Example 1:  Run `im3py` for all years using a configuration file
```python
from im3py.model import Model

run = Model(config_file="<path to your config file with the file name and extension.")

run.run_all_steps()
```

### Example 2:  Run `im3py` for all years by passing argument values
```python
from im3py.model import Model

run = Model(output_directory="<output directory path>", 
            start_step=2015,
            through_step=2016,
            time_step=1,
            alpha_param=2.0,
            beta_param=1.42,
            write_logfile=False)

run.run_all_steps()
```

### Example 3:  Run `im3py` by year by passing argument values; update value in between time step
```python
from im3py.model import Model

run = Model(output_directory="<output directory path>", 
            start_step=2015,
            through_step=2016,
            time_step=1,
            alpha_param=2.0,
            beta_param=1.42,
            write_logfile=False)

# initialize model
run.initialize()

# downscale year 0
run.advance_step()

# update the calibrated alpha parameter value
run.alpha_param = -0.1

# run next step with modified parameters
run.advance_step()

# close out run
run.close()
```

### Example 4:  Install supplemental data from a remote data source
```python
from im3py import InstallSupplement

dirpath = '<path to where you want to unpack the data>'

# instantiate class
sup = InstallSupplement(dirpath)

# fetch and unpack zipped data
sup.fetch_unpack_data()
```
