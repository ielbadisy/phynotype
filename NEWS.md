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

