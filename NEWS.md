# phynotype 0.6.4.9000

* `validate()` now honours `scale = TRUE`: internal metrics are computed on the scaled data the fit used, and bootstrap refits are scaled the same way (previously both used the raw data). Metric values change for scaled fits. `plotsilhouette()` uses the same basis.
* Soft cluster scores for distance-based methods (used by `ceterisparibus()`, `limeexplain()` and `featureimportance()` with `target = "score"`) now use a bandwidth fixed at fit time (median training distance to the centers, stored in `extras$score_sigma`). Previously the bandwidth was the median distance of the rows in the current call, so a row's score depended on the other rows scored with it. Score values change.
* `plotdendrogram()` now works on `"agnes"` fits (it uses the `hclust` copy stored in `extras$hclust`).
* `centers()` for `"gmm"` fits now returns a K x p matrix (one row per cluster) like k-means; it was p x K.
* `"dbscan"` fits now fill `extras$core_points` with the indices of core points (at least `minPts` points, itself included, within `eps`); it was always empty.
* `"hclust"` and `"agnes"` fits on a `dist` input now record the true metric in `distance_info$metric` (the `"Metric"` attribute of a `daisy`/`mixeddistance()` object, the `"method"` attribute of a `stats::dist()` object, else `"precomputed"`); it was always `"euclidean"`. `"pam"` and `"dbscan"` use the same rule.
* `prototypes()` no longer returns `NULL` for centroid fits and `metacluster()` fits. For k-means, GMM and hierarchical fits it returns the observed row nearest to each center; for a `metacluster_fit` it returns the consensus medoid of each cluster (smallest mean consensus dissimilarity to the other members). Row indices are in the `"row_ids"` attribute.
* `validate()` gains an explicit `seed` argument: on raw data every fit in the `k` grid and its bootstrap refits use it, so `validate(X, method, k, seed = 1)` is reproducible. Previously the seed could only be passed through `...`.
* `validate()`'s `per_cluster_table` now has columns `cluster`, `size` and `mean_silhouette`; the silhouette column was named `x`.
* `metacluster()` chooses the number of consensus clusters by a new rule: the `k` whose consensus cut has the highest mean adjusted Rand index with the candidate partitions, among cuts whose smallest cluster has at least `min_size` observations (default `max(2, ceiling(0.01 * n))`), ties to the smaller `k`. The previous rule (mean silhouette on `1 - C`) grows with `k` because the consensus is built from the large-`k` candidates too, so it tended to pick the upper end of the grid, sometimes with a one-observation cluster. New arguments `select = c("ari", "silhouette")` and `min_size`; `select = "silhouette"` restores the old criterion (with the size guard). `selection_summary` gains `mean_ari`, `min_size` and `eligible`. The selected `k` and the final clusters can change.
* `validate()` bootstrap ARI now compares the reference and bootstrap partitions on the distinct resampled rows only; duplicated draws made it optimistic (Monti et al., 2003). Values are lower than before. The standard deviation over resamples is added to the metrics table as `bootstrap_ari_sd`.

# phynotype 0.6.4

* `cluster()` now accepts a `dist` object for `method = "pam"` (via `cluster::pam(diss = TRUE)`) and `method = "dbscan"`, giving a Gower plus PAM path for mixed data with `mixeddistance()`. Fits on a `dist` object cannot predict new data and say so.
* Documented the name clash between `phynotype::cluster()` and `survival::cluster()` in `?cluster`, the README and the vignette; use `phynotype::cluster()` when both packages are attached.

# phynotype 0.6.3

Exported functions are renamed to glued lowercase (no underscores). The old names remain as deprecated wrappers for one release (see `?"phynotype-deprecated"`). S3 generics are renamed together with their methods (for example `plot_biplot()` and `plot_biplot.cluster_fit()` become `plotbiplot()` and `plotbiplot.cluster_fit()`). Class names (`cluster_fit`, `metacluster_fit`, `feature_importance`, `ceteris_paribus`, ...) and result fields such as `$n_clusters` are unchanged.

| Old | New |
|---|---|
| `adjusted_rand_index()` | `adjustedrandindex()` |
| `ceteris_paribus()` | `ceterisparibus()` |
| `feature_importance()` | `featureimportance()` |
| `lime_explain()` | `limeexplain()` |
| `method_used()` | `methodused()` |
| `mixed_distance()` | `mixeddistance()` |
| `n_clusters()` | `nclusters()` |
| `plot_biplot()` | `plotbiplot()` |
| `plot_cluster_sizes()` | `plotclustersizes()` |
| `plot_clusters()` | `plotclusters()` |
| `plot_coassoc()` | `plotcoassoc()` |
| `plot_consensus()` | `plotconsensus()` |
| `plot_dendrogram()` | `plotdendrogram()` |
| `plot_feature_profiles()` | `plotfeatureprofiles()` |
| `plot_silhouette()` | `plotsilhouette()` |
| `prepare_mixed_data()` | `preparemixeddata()` |
| `scale_color_phynotype()` | `scalecolorphynotype()` |
| `scale_fill_phynotype()` | `scalefillphynotype()` |
| `theme_phynotype()` | `themephynotype()` |

