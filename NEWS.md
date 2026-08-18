# phynotype 0.6.0

- Added `plot_biplot()`, a thin wrapper around `factoextra::fviz_pca_biplot()`/`fviz_mca_biplot()` for `"pca"` and `"mca"` embeddings, coloring individuals by cluster assignment. No FAMD or MDS equivalent, matching what `factoextra` itself provides.
- Added `theme_phynotype()`, a single ggplot2 theme now used consistently across every plot function in the package (previously some used `theme_minimal()`, one used `theme_bw()`, and `plot(ceteris_paribus)` had its own ad hoc purple-tinted theme).
- Fixed `metacluster()` so it can combine native mixed-data methods (`kproto`, `kmm`) with numeric-only (`kmeans`, `pam`, `dbscan`, `gmm`) or distance-based (`hclust`, `agnes`) methods on the same mixed-type dataset, by preparing per-method input instead of passing raw mixed data to every candidate.
- Fixed `validate.metacluster_fit()` to encode mixed-type data before computing silhouette/Calinski-Harabasz/Davies-Bouldin, matching `validate.cluster_fit()`.
- `print.cluster_validation()` now rounds the `value` column to 4 significant digits.
- First CRAN submission.

# phynotype 0.0.9

- Validation tables now include metric scale and preferred direction metadata for silhouette, bootstrap ARI, Davies-Bouldin, total within-cluster sum of squares, and Calinski-Harabasz.
- `plot_validation()` now facets metrics into individual panels instead of combining incompatible scales on one axis.
- Added the initial `cluster()` workflow with `kmeans`, `pam`, `hclust`, `agnes`, `dbscan`, and `gmm` wrappers.
- Added `metacluster()` with co-association consensus clustering.
- Added `validate()`, `explore()`, `predict()`, and core plotting helpers.
- Added package tests, documentation, and starter vignettes.
