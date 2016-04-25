// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <dplyr.h>
#include <Rcpp.h>

using namespace Rcpp;

// intersect_impl
Rcpp::DataFrame intersect_impl(DataFrame df_a, DataFrame df_b);
RcppExport SEXP Rbedtools_intersect_impl(SEXP df_aSEXP, SEXP df_bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type df_a(df_aSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type df_b(df_bSEXP);
    __result = Rcpp::wrap(intersect_impl(df_a, df_b));
    return __result;
END_RCPP
}
// merge_impl
DataFrame merge_impl(GroupedDataFrame gdf, int max_dist);
RcppExport SEXP Rbedtools_merge_impl(SEXP gdfSEXP, SEXP max_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GroupedDataFrame >::type gdf(gdfSEXP);
    Rcpp::traits::input_parameter< int >::type max_dist(max_distSEXP);
    __result = Rcpp::wrap(merge_impl(gdf, max_dist));
    return __result;
END_RCPP
}
// random_impl
DataFrame random_impl(DataFrame genome, int length, int n, unsigned int seed);
RcppExport SEXP Rbedtools_random_impl(SEXP genomeSEXP, SEXP lengthSEXP, SEXP nSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type genome(genomeSEXP);
    Rcpp::traits::input_parameter< int >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seed(seedSEXP);
    __result = Rcpp::wrap(random_impl(genome, length, n, seed));
    return __result;
END_RCPP
}
