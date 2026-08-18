# phynotype 0.6.0

- Added `plot_biplot()`, overlaying variable (or, for MCA, category) loading arrows on the PCA/FAMD/MCA embedding.
- Added `plot_cluster_network()`, a compact cluster-level network view with nodes at cluster centroids and edges weighted by centroid similarity.
- Fixed `metacluster()` so it can combine native mixed-data methods (`kproto`, `kmm`) with numeric-only (`kmeans`, `pam`, `dbscan`, `gmm`) or distance-based (`hclust`, `agnes`) methods on the same mixed-type dataset, by preparing per-method input instead of passing raw mixed data to every candidate.
- Fixed `validate.metacluster_fit()` to encode mixed-type data before computing silhouette/Calinski-Harabasz/Davies-Bouldin, matching `validate.cluster_fit()`.
- First CRAN submission.

# phynotype 0.0.9

- Validation tables now include metric scale and preferred direction metadata for silhouette, bootstrap ARI, Davies-Bouldin, total within-cluster sum of squares, and Calinski-Harabasz.
- `plot_validation()` now facets metrics into individual panels instead of combining incompatible scales on one axis.
- Added the initial `cluster()` workflow with `kmeans`, `pam`, `hclust`, `agnes`, `dbscan`, and `gmm` wrappers.
- Added `metacluster()` with co-association consensus clustering.
- Added `validate()`, `explore()`, `predict()`, and core plotting helpers.
- Added package tests, documentation, and starter vignettes.
