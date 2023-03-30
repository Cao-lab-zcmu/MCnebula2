---
contributors:
- LiChuang Huang
title: "Quick start"
date: "2023 Mar 23 15:34:19 | Thu"
lastmod: "[writeTime]: 12:11:23 2023-03-24"
draft: false
images: []
menu:
  docs:
    parent: "prologue"
weight: 50
toc: true
---



The following described:

<figure>
<center><img src="/docs/prologue/procedure_quick_start.svg"></center>
<center><figcaption>Main steps of MCnebula workflow</figcaption></center>
</figure>

## Samples

It may be a sample of any mixture of small molecule compounds, such as animal
metabolites, plant metabolites, drug samples, etc.

## LC-MS/MS

For the workflow, SIRIUS has many restrictive
[**Prerequisites**](https://boecker-lab.github.io/docs.sirius.github.io/prerequisites/)
that prevent its generalization to the analysis of any mass spectrometry data.
Fortunately, your data as least will not be rejected by one vote as long as
they meet the following two characteristics:

- High mass accuracy data
- Data-Dependent Acquisition (DDA) Mode.

## Convert raw data

GNPS Server provides detailed instructions and services regarding data
conversion.
[Click here](https://ccms-ucsd.github.io/GNPSDocumentation/fileconversion/).

If you are on Windows and have a lot of data to convert, then downloading
locally and using MSconvert might be the best option.
[Click here](https://proteowizard.sourceforge.io/download.html)

<figure>
<center><img src="/docs/prologue/MSconvert.png"></center>
<center><figcaption>An instance for setting MSconvert</figcaption></center>
</figure>

If you were not on Windows, installing the docker to perform MSconvert is
an option.
[`massconverter` of TidyMass](https://book.tidymass.org/massconverter.html)
provides a way in R to perform MSconvert with docker.

## Feature detection

<figure>
<center><img src="/docs/prologue/feature_detection.svg" width="100%"></center>
<center><figcaption>Pre-processing of LC-MS/MS</figcaption></center>
</figure>

Feature Detection is a kind of algorithm for detecting peaks from EIC plots,
and most mass spectrometry processing tools have a similar function. Users can
implement this process with any tool, but to access the MCnebula workflow, .mgf
(long list file containing MS/MS information) and .csv files (Feature
quantification table) are required for output. The following are some examples
of the four implementations of Feature Detection with output of .mgf files and
.csv:

### (Option 1) With MZmine

[MZmine](http://mzmine.github.io/) is a flexible deployment software for
processing LC-MS data, providing a range of algorithms that can be freely
combined to process LC-MS data according to user needs; its high degree of
flexibility makes it potentially difficult to use.

<figure>
<center><img src="/docs/prologue/mzmine3_logo.png"></center>
<center><figcaption>MZmine3</figcaption></center>
</figure>

MZmine has gone through several version changes and now MZmine3 is available.
Unfortunately, our next example still uses version 2.53; since we did a test
application with our Waters data when the original MZmine3 was released, only
to find that errors occurred during data import, we are not sure if the latest
version of MZmine3 has resolved this issue.

<figure>
<center><img src="/docs/prologue/gui_mzmine2.png"></center>
<center><figcaption>The GUI of MZmine2 (2.53)</figcaption></center>
</figure>

**Download MZmine with version 2.53**.
[Click here](https://github.com/mzmine/mzmine2/releases/tag/v2.53)

The **Guidance in GNPS** provides a step by step description about MZmine for
feature detection.
[Click here](https://ccms-ucsd.github.io/GNPSDocumentation/featurebasedmolecularnetworking-with-mzmine2/)

Using batch mode of MZmine is the most convenient way to repeat a given process
and parameters. We have provided some batch schema files (.xml) for MZmine2 for
example. [Click here](#demo-xml-of-batch-mode)

**Note** that different instruments and conditions may produce very different
data, so it is a necessary step to pre-study your own data and customize the
parameters when performing batch mode; ignore the parameter settings for the
main steps, at least when applying these demo batch mode files, you will need
to modify the input specified files and the output file names.

<figure>
<center><img src="/docs/prologue/mzmineBachMode_list_with_outputStep.png"></center>
<center><figcaption>An instance of batch queue for MZmine</figcaption></center>
</figure>

#### Demo XML for batch mode

- [Waters Qtof (with output step)](https://github.com/Cao-lab-zcmu/MZmine2_bachModeXML/blob/master/waters_with_output.xml)
- [Waters Qtof](https://github.com/Cao-lab-zcmu/MZmine2_bachModeXML/blob/master/waters.xml)
- [Thermo Orbitrap](https://github.com/Cao-lab-zcmu/MZmine2_bachModeXML/blob/master/thermo.xml)

### (Option 2) With XCMS

Preparing...

### (Option 3) With OpenMS

Preparing...

### (Option ...) Any tools

You can use any tool to export a list containing MS/MS information and .csv.
However, please **Note that** please convert the list of MS/MS information into a
format that matches the
[SIRIUS input](https://boecker-lab.github.io/docs.sirius.github.io/io/#input).

## Run SIRIUS

[SIRIUS](https://bio.informatik.uni-jena.de/software/sirius/) is a java-based
software framework for the analysis of LC-MS/MS data of
metabolites and other "small molecules of biological interest" ...

<figure>
<center><img src="/docs/prologue/sirius_gui.png"></center>
<center><figcaption>The GUI of SIRIUS 5</figcaption></center>
</figure>

### Sign up for use

Now, users must register to access from the SIRIUS network services (free for
academic use) (sign up and login in the GUI).

<figure>
<center><img src="/docs/prologue/sirius_signUp.png"></center>
<center><figcaption>Sign up to use SIRIUS 5</figcaption></center>
</figure>

### Import .mgf

The .mgf file is obtained from the processing of the Feature Detection in the
previous step, which records the basic information of the MS/MS. By dragging M
into the GUI of SIRIUS, the data will be imported successfully. For command
line mode, please refer to:
[Click here](https://boecker-lab.github.io/docs.sirius.github.io/quick-start/#command-line-interface)

The following is the content of a demo .mgf:

```
BEGIN IONS
FEATURE_ID=1
PEPMASS=532.30509842717
CHARGE=+1
MSLEVEL=1
532.30509842717 100
532.305646812001 100
533.309001652001 27.0393207318305
END IONS

BEGIN IONS
FEATURE_ID=gnps1
PEPMASS=532.30509842717
CHARGE=+1
RTINSECONDS=
MSLEVEL=2
153.8885 148
181.8 74
334.8798 1881
360.7695 139
514.1035 120
451.17258 18.81
END IONS

```

### Activate modules

Activate SIRIUS, ZODIAC, CSI:FingerID and CANOPUS for computation.
(If necessary, adjust the parameters)

<figure>
<center><img src="/docs/prologue/activate_sirius.png"></center>
<center><figcaption>Activate SIRIUS, ZODIAC, CSI:FingerID and CANOPUS</figcaption></center>
</figure>

### Write summaries

It may take hours or even an evening to calculate the LC-MS/MS data set
containing thousands of Features. When it is over, write a summary.

<figure>
<center><img src="/docs/prologue/summary_sirius.png"></center>
<center><figcaption>Write Summary while Jobs finished</figcaption></center>
</figure>

## MCnebula2

Now, let's get started with the R package MCnebula2!


```r
## The `path` is where your SIRIUS project saved.
path <- "."
mcn <- mcnebula()
mcn <- initialize_mcnebula(mcn, "sirius.v5", path)
```

&lt;&lt;&lt; [Workflow](/docs/workflow/) &gt;&gt;&gt;