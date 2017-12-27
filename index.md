# hm
=============

Write Graphviz `dot` files for archaeological sequence diagrams and chronological models.  Convert a sequence diagram to a chronological model.

## Requirements

The `hm` package is Common Lisp software that runs on [SBCL](http://www.sbcl.org/) or [Clozure CL](http://ccl.clozure.com/).

It is convenient to install the software with [the `git` distributed version control system](http://git-scm.com/).

The output file produced by the `hm` package is source code for [Graphviz `dot`](http://www.graphviz.org/).
 
## Install

Installation with [Quicklisp](http://www.quicklisp.org) is recommended.  It automatically installs [dependencies](http://eschulte.github.io/graph/).

`cd ~/quicklisp/local-projects/`

`git clone https://github.com/tsdye/harris-matrix.git`

## Load

`(ql:quickload "hm")`

`(in-package hm)`

## Draw

`(hm-draw "path/to/configuration/file")`

## Overview

The `hm` package produces Graphviz `dot` source code for archaeological sequence diagrams from tables of stratigraphic observations, inferences, and interpretations.  With the addition of a table of events, the `hm` package will produce a Bayesian chronological model.

The `hm` package provides a single function, `hm-draw`, which expects a valid path to a configuration file as its sole argument.

The configuration file specifies paths to one or more data files and sets values for several dozen Graphviz `dot` variables.

The configuration information and the data are held in comma separated value files.  Together, the configuration and data files can hold a complete description of site stratigraphy and its interpretation. 

## Example Output

The `hm` package includes several example configuration and data files.

The stratigraphic section shown in Figure 12 of Edward Harris' book, _Principles of Archaeological Stratigraphy_.  Note that the symbol for a deposit is a box and for an interface a trapezium. In this example, the nodes are colored according to the directed graph concept of _level_.

![fig-12-harris-levels-tred.svg](http://harris-matrix.tsdye.com/img/fig-12-harris-levels-tred.svg)

Another view of the stratigraphic section shown in Figure 12 of Edward Harris' book, _Principles of Archaeological Stratigraphy_.  In this example, layer interfaces are recognized and the nodes are colored to reflect the relationship of Context 4 to other contexts in a Bayesian chronological model.

![fig-12-distance-from-4-layer-interfaces-set24-tred.svg](http://harris-matrix.tsdye.com/img/fig-12-distance-from-4-layer-interfaces-set24-tred.svg)

"Avoiding a common error in Harris matrix construction," is an example H-structure from Figure 14 of Steve Roskams' book, _Excavation_.  You can draw this diagram as follows:

`(hm-draw "path/to/examples/roskams-h-structure/roskams-h-cnf.csv")`

Note that you might have to adjust the paths in `roskams-h-cnf.csv` to reflect your file structure.

![Roskams' H Structure](http://harris-matrix.tsdye.com/img/roskams-h-tred.svg)

A complex H-structure from an article by Irmela Herzog and Irwin Scollar, "A new graph theoretic oriented program for Harris Matrix analysis," with nodes colored according to reachability from Context 2.  The node coloring resolves potential ambiguities in the diagram.

![Roskams's Fig. 9.5](http://harris-matrix.tsdye.com/img/hs95a-reachable-from-2-tred.svg)

Another view of the complex H-structure with nodes colored according to reachability from Context 4.

![Roskams's Fig. 9.5](http://harris-matrix.tsdye.com/img/hs95a-reachable-from-4-tred.svg)

Sequence diagrams sometimes require "jumps" or "crossings".  This example from Figure 13 of Steve Roskams' book, _Excavation_, was created with the following command:

`(hm-draw "path/to/examples/roskams-jumps/roskams-cnf.csv")`

Note that you might have to adjust the paths in `roskams-cnf.csv` to reflect your file structure.

![Roskams' jumps](http://harris-matrix.tsdye.com/img/roskams-jumps-tred.svg)
## Data Tables

There are seven data tables used as input to `hm`.  Examples of each are provided in the `hm` git repository.

### Units of Stratification

This table describes the contexts identified by the archaeologist.

-   **label:** context identifier (primary key)
-   **unit-type:** interface or deposit
-   **position:** surface/basal/other
-   **period:** period identifier (foreign key)
-   **phase:** phase identifier (foreign key)
-   **url:** node link (svg output only)

### Observed Stratigraphic Relations

This table includes observations of stratigraphic superposition.  Its
first two columns refer to values from the `label` column of the
contexts table.

-   **younger:** stratigraphically superior context label (foreign key)
-   **older:** stratigraphically inferior context label (foreign key)
-   **url:** arc link (svg output only)

### Inferred Parity Relations

This table contains inferences of parity between pairs of
discontiguous contexts.

-   **first:** context label (foreign key)
-   **second:** context label (foreign key)

### Periods

This table describes periods, which are groups of interfacial contexts believed to have been in use at the same time.

-   **id:** a unique integer to identify the period (primary key)
-   **label:** name of the period used in the legend
-   **color:** zero-based integer scale for a [Brewer color](http://www.graphviz.org/doc/info/colors.html#brewer)
-   **description:** optional field not used by `hm`

### Phases

This table describes phases, which are groups of depositional contexts believed to have been deposited pene-contemporaneously, typically because they share diagnostic artifactual content.

-   **id:** a unique integer to identify the phase (primary key)
-   **label:** name of the phase used in the legend
-   **color:** zero-based integer scale for a [Brewer color](http://www.graphviz.org/doc/info/colors.html#brewer)
-   **description:** optional field not used by `hm`

### Events

This table associates events and contexts and specifies the nature of the association using terms introduced to archaeology by Jeffrey Dean in 1978.

- **theta:** unique label (primary key)
- **context:** context identifier (foreign key)
- **lab:** dating laboratory identifier
- **association** one of `disjunct`, `direct`, `disparate`

### Event Order

This optional table contains information on the temporal order of events from the same context.

- **older:** theta label of older date (foreign key)
- **younger:** theta label of younger date (foreign key)


## Configuration File

An empty configuration file is included in the repository as examples/empty-configuration-file.csv.

In addition, each of the examples comes with its own configuration file.

# Plotting the Sequence Diagram

I have two shell scripts to automate graph plotting on my Mac OS X system.  They each take two arguments:
* path to the `dot` file, without file extension
* [output file type](http://www.graphviz.org/content/output-formats)

This one simply plots and opens the graph.  It will display all the arcs.

`dot -T$2 $1.dot -o $1.$2`

`open $1.$2`

This one plots and opens the transitive reduction of the graph.  Transitive reduction corresponds to Harris' Law of Stratigraphical Succession.

`tred $1.dot > $1-tred.dot`

`dot -T$2 $1-tred.dot -o $1-tred.$2`

`open $1-tred.$2`

# Workflow

Legends are displayed as elements; no attempt at good legend layout is made.  The `svg` output from `dot` can be edited with an application such as [Inkscape](http://www.inkscape.org/en/).
