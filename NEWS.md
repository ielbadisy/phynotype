# phynotype 0.6.2

- `plot_biplot()` no longer depends on `factoextra`. It's now a
  self-contained reimplementation of `fviz_pca_biplot()`/`fviz_mca_biplot()`'s
  geometry (individual points/arrows, variable arrows or category points,
  axis percentage labels, dashed origin lines, the `r * 0.7` variable-arrow
  scaling rule, and the exact cos2/contribution formulas `factoextra` uses
  for `prcomp`/`FactoMineR::MCA` objects), verified to reproduce
  numerically identical coordinates. `factoextra` dropped from `Suggests`;
  `ggrepel` added to `Imports` (previously an indirect `factoextra`
  dependency, now used directly for repelled labels).

# phynotype 0.6.1

- Standardized figure colors package-wide. `theme_phynotype()` now builds on
  `ggplot2::theme_classic()` with an Okabe-Ito colorblind-safe palette
  (matching the `funcml` package's `theme_funcml()`), and new
  `scale_color_phynotype()`/`scale_fill_phynotype()` discrete scales are
  applied to every plot with a categorical color/fill mapping (cluster
  embedding, consensus clusters, feature profiles, silhouette widths).
  Previously these fell back to ggplot2's default hue palette.
- `plot(ceteris_paribus)` no longer uses its own ad hoc teal/purple colors
  (visually indistinguishable from the `DALEX`/`ingredients` package's
  ceteris-paribus plot style, despite no dependency on it) - it now uses the
  same blue/vermillion accent pair as the rest of the package.
- `plot_coassoc()`'s co-association heatmap now uses a proper continuous
  `viridis` gradient instead of ggplot2's default blue gradient.
- `plot(lime_explanation)`'s positive/negative bars now use a consistent
  green/vermillion pair instead of ggplot2's default hue palette.
- The two independently hardcoded feature-importance/cluster-size bar colors
  are now sourced from the same shared accent constant.
- README figures re-rendered to reflect the new palette.

# phynotype 0.6.0

- Added `plot_biplot()`, a thin wrapper around `factoextra::fviz_pca_biplot()`/`fviz_mca_biplot()` for `"pca"` and `"mca"` embeddings. No FAMD or MDS equivalent, matching what `factoextra` itself provides. Three `variant`s, all following `factoextra`'s own documented examples: `"cluster"` (default, colored/shaped by cluster assignment), `"cos2"` (colored by quality of representation), and `"label"` (individuals shown as text instead of points).
- Added `theme_phynotype()`, a single ggplot2 theme now used consistently across every plot function in the package (previously some used `theme_minimal()`, one used `theme_bw()`, and `plot(ceteris_paribus)` had its own ad hoc purple-tinted theme).
- Fixed `metacluster()` so it can combine native mixed-data methods (`kproto`, `kmm`) with numeric-only (`kmeans`, `pam`, `dbscan`, `gmm`) or distance-based (`hclust`, `agnes`) methods on the same mixed-type dataset, by preparing per-method input instead of passing raw mixed data to every candidate.
- Fixed `validate.metacluster_fit()` to encode mixed-type data before computing silhouette/Calinski-Harabasz/Davies-Bouldin, matching `validate.cluster_fit()`.
- All numeric package results now round to 4 significant digits by default: `validate()`'s metrics/per-cluster tables, `explore()`'s feature summary/separation/prototype tables, `centers()`/`prototypes()`, `feature_importance()`, `ceteris_paribus()`, `lime_explain()`, and `metacluster()`'s selection/stability summaries.
- First CRAN submission.

# phynotype 0.0.9

- Validation tables now include metric scale and preferred direction metadata for silhouette, bootstrap ARI, Davies-Bouldin, total within-cluster sum of squares, and Calinski-Harabasz.
- `plot_validation()` now facets metrics into individual panels instead of combining incompatible scales on one axis.
- Added the initial `cluster()` workflow with `kmeans`, `pam`, `hclust`, `agnes`, `dbscan`, and `gmm` wrappers.
- Added `metacluster()` with co-association consensus clustering.
- Added `validate()`, `explore()`, `predict()`, and core plotting helpers.
- Added package tests, documentation, and starter vignettes.
