#include <Rcpp.h>

using namespace Rcpp;

//
// *** MARKETS ***
//

List calc_market_priceC (double pd, double pg) {
    
    return List::create(_["grey"] = pd , _["green"] = pg);
}// --------------------    END OF FUNCTION calc_market_priceC      --------------------###

List calc_market_quantityC (List params) {
    double qg = as<double>(params["market_size"]) * as<double>(params["market_prop_green"]);
    double qd = as<double>(params["market_size"]) - qg;
    
    return List::create(_["grey"] = qd , _["green"] = qg);
}// --------------------    END OF FUNCTION calc_market_quantityC   --------------------###

NumericVector dist_market_quantityC(NumericVector max_units, double total_units) {
    NumericVector distribution(max_units.size());
    NumericVector nunits(max_units.size());
    double excess = total_units;

    if (std::accumulate(max_units.begin(), max_units.end(), 0) < total_units) {
        nunits = max_units;
    }

    while (excess>0 && is_true(any(nunits<max_units))) {
        NumericVector remn = ifelse(nunits<max_units, 1.0, 0.0);
        nunits = nunits + remn*(excess / sum(remn));
        nunits = pmin(nunits, max_units);

        excess = total_units - sum(nunits);
    }

    return nunits;
}// --------------------    END OF FUNCTION dist_market_quantityC   --------------------###

//
// *** FIRM VALUATION ***
//

// [[Rcpp::export]]
List calc_revenueC (DataFrame agents, List params) {
    NumericVector gas_output = agents["gas_output"];
    NumericVector mitigation = sapply(as<CharacterVector>(agents["behavior"]), [](String x) -> double { return x!="flaring"; });

    List prices = calc_market_priceC(params["market_price_grey"], params["market_price_green"]);
    List total_units = calc_market_quantityC(params);
    NumericVector green_units = dist_market_quantityC(gas_output * mitigation,  total_units["green"]);
    NumericVector grey_units = dist_market_quantityC(gas_output - green_units, total_units["grey"]);
    double green_coeff = (as<double>(total_units["green"]) - sum(green_units)) / (as<double>(total_units["grey"]) + as<double>(total_units["green"]));
    green_coeff *= as<double>(prices["green"]) - as<double>(prices["grey"]);
    return List::create(_["gas_revenue"] = (as<double>(prices["green"]) * green_units) + (as<double>(prices["grey"]) * grey_units) ,
                        _["green_units"] = green_units, _["prices"] = prices  , _["green_coeff"] = green_coeff);
}// --------------------    END OF FUNCTION calc_revenueC           --------------------###

// [[Rcpp::export]]
double calc_netm_costC (DataFrame agent_options, double SRoR) {
    // calculate the net cost of mitigating: change in cost less change in revenue
    NumericVector m = 2*(as<NumericVector>(agent_options["meets_thresh"])) - 1;
    NumericVector  cost = m * as<NumericVector>(agent_options["cost_M_add"]),
                revenue = m * as<NumericVector>(agent_options["gas_revenue"]);    

    return (std::accumulate(cost.begin(), cost.end(), 0.0) * (1 - SRoR)) - 
            std::accumulate(revenue.begin(), revenue.end(), 0.0);
}// --------------------    END OF FUNCTION calc_netm_costC         --------------------###

//
// *** PORTFOLIO ***
//

void vector_permutationsC(List array, int i, IntegerVector accum, List& perm, int& k) {
    // recursive function to determine all possible permutations of arbitrarily many
    //   vectors of arbitrary size. Each vector is an element in the List array
    if (i == array.size()) {
        // done, no more rows
        perm[k] = clone(accum);
        k += 1;
    }
    else {
        IntegerVector row = array[i];
        for(int j = 0; j < row.size(); ++j) {
            accum[i] = row[j];
            vector_permutationsC(array, i+1, accum, perm, k);
        }
    }
}// --------------------    END OF FUNCTION vector_permutationsC    --------------------###

// [[Rcpp::export]]
List class_permutationsC(CharacterVector lease_classes) {
    // calculate all valid permutations of how fields can evolve based on class names
    // a lease's class can only increase (IE underdeveloped --> developed, developed -/-> underdeveloped)
    // doing nothing (0) is always an option
    List class_options (lease_classes.size(), IntegerVector{0});
    int output_dim = 0;

    for(int i = 0; i < lease_classes.size(); i++) {
        if (lease_classes[i] != "developed") {
            // underdeveloped leases can also be further developed (1)
            class_options[i] = IntegerVector{0, 1};
            output_dim += 1;
        }
    }

    // recursively determine all possible permutations of development vectors
    List perm (pow(2, output_dim));
    int k = 0;
    vector_permutationsC(class_options, 0, IntegerVector(lease_classes.size()), perm, k);

    return perm;
}// --------------------    END OF FUNCTION class_permutationsC     --------------------###

