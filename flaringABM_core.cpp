#include <Rcpp.h>

using namespace Rcpp;
//
// *** SOCIAL PRESSURE ***
//
// double calc_total_pressureC () {
//     List params = Environment::global_env()["Params"];
    
//     return params["Activism"];
// }


//
// *** MARKETS ***
//

List calc_market_priceC (double pd, double pg) {
    
    return List::create(_["dirty"] = pd , _["green"] = pg);
}// --------------------    END OF FUNCTION calc_market_priceC      --------------------###

List calc_market_quantityC (double time) {
    List params = Environment::global_env()["Params"];
    double qg = as<double>(params["market_size"]) * 0.02;
    double qd = as<double>(params["market_size"]) - qg;
    
    return List::create(_["dirty"] = qd , _["green"] = qg);
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
NumericVector calc_costC (DataFrame agents, double time, NumericVector t_switch=NumericVector()) {
    //Determine the cost at "time", assuming the firm transitions to the green market at "switch_time"
    NumericVector switch_time;
    if (t_switch.size()==0) {
        switch_time = agents["t_switch"];
    } else {
        switch_time = NumericVector(agents.nrow(), t_switch[0]);
    }

    NumericVector green_fCost = agents["green_fCost"], oCost = agents["oCost"], i_horizon = agents["i_horizon"];
    NumericVector green_add_oCost = ifelse(is_na(switch_time), 0.0, as<NumericVector>(agents["green_add_oCost"]));
    //LogicalVector cost_binary = ifelse(is_na(switch_time), 0.0, switch_time < i_horizon - time);
    LogicalVector cost_binary = ifelse(is_na(switch_time), 0.0, switch_time + i_horizon > time);
    
    //assume the fixed cost is paid off equally over i_horizon years
    return (as<NumericVector>(cost_binary) * green_fCost / i_horizon) + oCost + green_add_oCost;
}// --------------------    END OF FUNCTION calc_costC              --------------------###

// [[Rcpp::export]]
NumericVector calc_revenueC (DataFrame agents, double time) {
    NumericVector capacity = agents["capacity"], mitigation = agents["mitigation"];
    List params = Environment::global_env()["Params"];

    List prices = calc_market_priceC(params["market_price_dirty"], params["market_price_green"]);
    List total_units = calc_market_quantityC(time);
    NumericVector green_units = dist_market_quantityC(capacity * floor(mitigation),  total_units["green"]);
    NumericVector dirty_units = dist_market_quantityC(capacity - green_units, total_units["dirty"]);

    return (as<double>(prices["green"]) * green_units) + (as<double>(prices["dirty"]) * dirty_units);
    
}// --------------------    END OF FUNCTION calc_revenueC           --------------------###

// [[Rcpp::export]]
LogicalVector check_affordabilityC (DataFrame buyers) {
    //check if each agent can afford the fixed cost of mitigation
    //naively assume there is no borrowing available and the cost is paid as a lump sum
    NumericVector green_fCost = buyers["green_fCost"], capital = buyers["capital"];

    return green_fCost < capital;
}// --------------------    END OF FUNCTION check_affordabilityC    --------------------###