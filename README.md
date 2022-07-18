# MOS gridding utilities

R code for producing gridded forecasts from MOS postprocesses point forecasts. Uses package [`fastgrid`](https://github.com/mjlaine/fastgrid) for fast Kriging.

See also [`MOSplotting`](https://github.com/fmidev/MOSplotting) for plotting and
[`post-processing-mos-point-analysis`](https://github.com/fmidev/post-processing-mos-point-analysis) for MOS point analysis.

Operational scripts are in [`POSSE_GRID`](https://github.com/fmidev/POSSE_GRID).

## Dependencies

Code reads grib files using packages from https://github.com/harphub/.

Basically you could install them directly like given below, but in most cases you need to check compiler flags related to the proj library.

```
remotes::install_github('harphub/meteogrid')
remotes::install_github('harphub/Rgrib2')
```

Questions to marko.laine@fmi.fi
