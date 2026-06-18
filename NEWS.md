# export 0.3.3

### Bug fixes

* Improved `graph2ppt()` font handling for plotmath/Greek symbols by forwarding
  font aliases to `rvg::dml()` and inferring ggplot text families when possible.
* Added a raster fallback for `graph2office()` when `showtext_auto()` is enabled.
* Made graph export size inference work without an active graphics device when a
  plot object or plotting function is supplied.
* Added direct coefficient-table export support for `splm` and `summary.splm`
  objects.

# export 0.3

### New features

* Fixed NOTE that resulted in some temporary files being left on non-interactive
Debian systems.

# export 0.2.2.9000

### New features

* Fixed bug when no graphical devices are available fro exporting graphs (as pointed out by guokai8 on GitHub)

# export 0.2.2 Beta

### New features

* Made export use officer and flextable rather than ReporteRs and rtable, as the latter wtwo ere removed from CRAN.
* Created table2spreadsheet() to generate table in a Microsoft Excel / LibreOffice Calc file or in a CSV (using either comma or semicolon as separation)
* table2spreadsheet() and table2office() use the "broom" package additionaly to the "xtable" package which allows for more data classes to be exported as tables

### Removed features

* Removed graph2tex() as it is better to use graph2bitmap or graph2pdf and to manually insert these in the LaTex document 


# export 0.2.1 Beta

### New features

* Created table2office() to generate tables in a presentation (Microsoft PowerPoint or LibreOffice) or in a document (Microsoft Word or LibreOffice) with similar behavior as graph2office().


# export 0.2.0 Beta

### New features

* Automatic selection of paper size based on graph size in MS Office export
* Added Latex export of graphs & tables
