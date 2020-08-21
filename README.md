# Im Zerrspiegel des Populismus
### Eine computergestützte Analyse der Verlinkungspraxis von Bundestagsabgeordneten auf Twitter
Dieses Repository bietet einige Daten und Skripte zum Paper:

* von Nordheim, G. & Rieger, J. (2020). Im Zerrspiegel des Populismus - Eine computergestützte Analyse der Verlinkungspraxis von Bundestagsabgeordneten auf Twitter. *Publizistik*. https://doi.org/10.1007/s11616-020-00591-7,
* siehe auch https://de.ejo-online.eu/digitales/die-kleine-Welt-des-Populismus.

Für Fragen und Anmerkungen verwendet bitte den [issue tracker](https://github.com/JonasRieger/zerrspiegel/issues).

#### Distorted by populism - A computational analysis of German parliamentarians’ linking practices on Twitter
This repository provides some data and scripts related to the paper:

* von Nordheim, G. & Rieger, J. (2020). Distorted by Populism – A computational analysis of German parliamentarians’ linking practices on Twitter [Im Zerrspiegel des Populismus – Eine computergestützte Analyse der Verlinkungspraxis von Bundestagsabgeordneten auf Twitter]. *Publizistik*. https://doi.org/10.1007/s11616-020-00591-7,
* see also https://de.ejo-online.eu/digitales/die-kleine-Welt-des-Populismus.

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/zerrspiegel/issues).

## Related Software
* [tosca](https://github.com/Docma-TU/tosca) is used for managing and manipulating the text data to a structure requested by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) is used to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [longurl](https://github.com/hrbrmstr/longurl) is used for expanding short urls.
* [urltools](https://github.com/Ironholds/urltools) is useful for extracting url cores from urls.
* [spelling](https://github.com/ropensci/spelling) is used for identifying non-German texts.
* [batchtools](https://github.com/mllg/batchtools) is used for calculating (prototypes of) LDAs on the High Performace Compute Cluster [LiDO3](https://www.lido.tu-dortmund.de/cms/en/LiDO3/index.html).
* [ineq](https://cran.r-project.org/package=ineq) is used to calcaulate Gini coefficients.
* [ggparliament](https://github.com/RobWHickman/ggparliament) is used for visualization of the parliament.
* [beanplot](https://cran.r-project.org/package=beanplot) is used for visualize advanced boxplots.

## Usage
Please note: For legal reasons the repository cannot provide all data. Please [let us know](https://github.com/JonasRieger/zerrspiegel/issues) if you feel that there is anything missing that we could add. 

Due to the limited possibility to provide the raw data, all created plots and CSV files are collected in the folders ``Plots`` and ``Parteiverlinkungen``. The scripts ``EDA.R`` (explorative data analysis), ``plotGini.R`` and ``plotLDA.R`` indicate the corresponding scripts. The script ``LDA.R`` shows how we calculated the different LDAs. All necessary data for the calculation are available. As an example we have stored the version with K = 30 Topics as result in the folder ``Modellieren``.
