# phynotype 0.0.9

- Validation tables now include metric scale and preferred direction metadata for silhouette, bootstrap ARI, Davies-Bouldin, total within-cluster sum of squares, and Calinski-Harabasz.
- `plot_validation()` now facets metrics into individual panels instead of combining incompatible scales on one axis.
- Added the initial `cluster()` workflow with `kmeans`, `pam`, `hclust`, `agnes`, `dbscan`, and `gmm` wrappers.
- Added `metacluster()` with co-association consensus clustering.
- Added `validate()`, `explore()`, `predict()`, and core plotting helpers.
- Added package tests, documentation, and starter vignettes.
