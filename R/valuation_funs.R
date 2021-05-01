#' Capitalize R&D (-2 years)
#' @description Capitalize R&D investments (Non-tech service, Ads, Banks, Restaurant, Retail Store etc.)
#' @param cashflows A list of numbers
#' @param amortized Boolean
#' @return Amortized or Unamortized portion of R&D
#' @examples
#' # Vector of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_2(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_2(cash_flow)
#' @export
get_amortization_2 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y2 <- cash_flows$y2 * 0
    un_y1 <- cash_flows$y1 * (1/2)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2)
    # Calculate amortized portion
  } else {
    am_y2 <- cash_flows$y2 * (1/2)
    am_y1 <- cash_flows$y1 * (1/2)
    current <- cash_flows$current * 0
    total <- as.integer(am_y2 + am_y1 + current)

  }
}

#' Capitalize R&D (-3 years)
#' @description Capitalize R&D investments (Retail tech service, Apparel, Entertainment, Food processing, Household products, Newspapers, Publishing etc.)
#' @param cashflows A list of numbers
#' @param amortized Boolean
#' @return Amortized or Unamortized portion of R&D
#' @examples
#' # Vector of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=555)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_3(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_3(cash_flow)
#' @export
get_amortization_3 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y3 <- cash_flows$y3 * 0
    un_y2 <- cash_flows$y2 * (1/3)
    un_y1 <- cash_flows$y1 * (2/3)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3)
    # Calculate amortized portion
  } else {
    am_y3 <- cash_flows$y3 * (1/3)
    am_y2 <- cash_flows$y2 * (1/3)
    am_y1 <- cash_flows$y1 * (1/3)
    current <- cash_flows$current * 0
    total <- as.integer(am_y3 + am_y2 + am_y1 + current)

  }
}

#' Capitalize R&D (-5 years)
#' @description Capitalize R&D investments (Light manufacturing, Aluminum, Auto Parts, Building Materials, Computer Peripherals, Gold/Silver, Mining, Office supplies, Petroleum, Textile, Rubber, Tabacco)
#' @param cashflows A list of numbers
#' @param amortized Boolean
#' @return Amortized or Unamortized portion of R&D
#' @examples
#' # Vector of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_5(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_5(cash_flow)
#' @export
get_amortization_5 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y5 <- cash_flows$y5 * 0
    un_y4 <- cash_flows$y4 * (1/5)
    un_y3 <- cash_flows$y3 * (2/5)
    un_y2 <- cash_flows$y2 * (3/5)
    un_y1 <- cash_flows$y1 * (4/5)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3 + un_y4 + un_y5)
    # Calculate amortized portion
  } else {
    am_y5 <- cash_flows$y5 * (1/5)
    am_y4 <- cash_flows$y4 * (1/5)
    am_y3 <- cash_flows$y3 * (1/5)
    am_y2 <- cash_flows$y2 * (1/5)
    am_y1 <- cash_flows$y1 * (1/5)
    current <- cash_flows$current * 0
    total <- as.integer(am_y5 + am_y4 + am_y3 + am_y2 + am_y1 + current)

  }
}


#' Capitalize R&D (-10 years)
#' @description Capitalize R&D investments (Chemical, Auto/Truck, Drug, Machine, Maritime, Paper, Telecom Equipment, Water Utility, Heavy Manufacturing, Research with patenting)
#' @param cashflows A list of numbers
#' @param amortized Boolean
#' @return Amortized or Unamortized portion of R&D
#' @examples
#' # Vector of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666, ...)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_10(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_10(cash_flow)
#' @export
get_amortization_10 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y10 <- cash_flows$y10 * 0
    un_y9 <- cash_flows$y9 * (1/10)
    un_y8 <- cash_flows$y8 * (2/10)
    un_y7 <- cash_flows$y7 * (3/10)
    un_y6 <- cash_flows$y6 * (4/10)
    un_y5 <- cash_flows$y5 * (5/10)
    un_y4 <- cash_flows$y4 * (6/10)
    un_y3 <- cash_flows$y3 * (7/10)
    un_y2 <- cash_flows$y2 * (8/10)
    un_y1 <- cash_flows$y1 * (9/10)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3 + un_y4 + un_y5 + un_y6 + un_y7 + un_y8 + un_y9 + un_y10)
    # Calculate amortized portion
  } else {
    am_y10 <- cash_flows$y10 * (1/10)
    am_y9 <- cash_flows$y9 * (1/10)
    am_y8 <- cash_flows$y8 * (1/10)
    am_y7 <- cash_flows$y7 * (1/10)
    am_y6 <- cash_flows$y6 * (1/10)
    am_y5 <- cash_flows$y5 * (1/10)
    am_y4 <- cash_flows$y4 * (1/10)
    am_y3 <- cash_flows$y3 * (1/10)
    am_y2 <- cash_flows$y2 * (1/10)
    am_y1 <- cash_flows$y1 * (1/10)
    current <- cash_flows$current * 0
    total <- as.integer(am_y10 + am_y9 + am_y8 + am_y7 + am_y6 + am_y5 + am_y4 + am_y3 + am_y2 + am_y1 + current)

  }
}


#' Capitalize future leases
#' @description Transform future paid leases into debt
#' @param lease_flow A list of numbers
#' @param beyond A number
#' @param cost_debt A number
#' @return List of Debt value of leases AND years embedded in lease (5Y)
#' @examples
#' # Paid leases 5Y in the future
#' cash_flow <- list(y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Paid leases beyond 5Y
#' beyond <- 45555
#' # Indicate the cost of debt (firm rating OR synthetic rating)
#' cost_debt <- 0.05 (5%)
#' # Capitalize future leases
#' cap_leases <- get_leases(cash_flow, beyond, cost_debt)
#' # Capitalized leases
#' cap_leases$debt_value_lease
#' # Embedded lease years
#' cap_leases$years_embedded
#' @export
get_leases <- function(lease_flow, beyond, cost_debt) {
  # Calculate embedded years beyond 6
  years_embedded <- round(beyond/mean(unlist(lease_flow)))
  if (beyond > 0) {
    if (years_embedded > 1) {
      # Cash below year 6
      print("Beyond value > 1")
      value_beyond <- beyond / years_embedded
      l_beyond <- ((value_beyond*(1-1/(1+cost_debt)**years_embedded))/cost_debt)/(1+cost_debt)**5
      l_y1 <- lease_flow$y1/(1+cost_debt)**1
      l_y2 <- lease_flow$y2/(1+cost_debt)**2
      l_y3 <- lease_flow$y3/(1+cost_debt)**3
      l_y4 <- lease_flow$y4/(1+cost_debt)**4
      l_y5 <- lease_flow$y5/(1+cost_debt)**5
      l_total <- as.integer(l_beyond + l_y1 + l_y2 + l_y3 + l_y4 + l_y5)
      result <- list(debt_value_lease=l_total, years_embedded=years_embedded)
    } else {
      # Years embedded = 1
      print("Beyond value = 1")
      l_beyond <- beyond / (1+cost_debt)**6
      l_y1 <- lease_flow$y1/(1+cost_debt)**1
      l_y2 <- lease_flow$y2/(1+cost_debt)**2
      l_y3 <- lease_flow$y3/(1+cost_debt)**3
      l_y4 <- lease_flow$y4/(1+cost_debt)**4
      l_y5 <- lease_flow$y5/(1+cost_debt)**5
      l_total <- as.integer(l_beyond + l_y1 + l_y2 + l_y3 + l_y4 + l_y5)
      result <- list(debt_value_lease=l_total, years_embedded=years_embedded)
    }
  } else {
    print("No beyond value")
      l_y1 <- lease_flow$y1/(1+cost_debt)**1
      l_y2 <- lease_flow$y2/(1+cost_debt)**2
      l_y3 <- lease_flow$y3/(1+cost_debt)**3
      l_y4 <- lease_flow$y4/(1+cost_debt)**4
      l_y5 <- lease_flow$y5/(1+cost_debt)**5
      l_total <- as.integer(l_y1 + l_y2 + l_y3 + l_y4 + l_y5)
      result <- list(debt_value_lease=l_total, years_embedded=years_embedded)
  }
}

#' Calculate lease adjustment to EBIT
#' @description Calculate lease adjustment to EBIT (for ROIC calculation)
#' @param current_lease A number Current lease expense (from IS)
#' @param debt_value_lease A number Capitalized leases
#' @param lease_embedded_years A number Embedded years in leases (5Y)
#' @return A number of lease adjustment for EBIT
#' @examples
#' See get_leases function output for debt_value_lease and lease_embedded_years
#' # Current year leases
#' current_lease <- 555
#' lease_adj <- lease_adjustment(current_lease, get_leases$debt_value_lease, get_leases$years_embedded)
#' @export
lease_adjustment <- function(current_lease, debt_value_lease, lease_embedded_years) {
  result <- as.integer(current_lease - debt_value_lease / (lease_embedded_years+5))
}

#' Calculate R&D adjustment to EBIT
#' @description Calculate R&D adjustment to EBIT (for ROIC calculation)
#' @param current_rnd A number Current year R&D expense
#' @param amortized_rnd A number Amortized R&D
#' @return A number of R&D adjustment for EBIT
#' @examples
#' See get_amortization function output for amortized R&D
#' # Current year R&D
#' current_rnd <- 1771
#' amortized_rnd <- get_amortization_3(...)
#' lease_adj <- rnd_adjustment(current_rnd, amortized_rnd)
#' @export
rnd_adjustment <- function(current_rnd, amortized_rnd) {
  result <- as.integer(current_rnd-amortized_rnd)
}

#' Calculate ROIC
#' @description Calculate Return on Invested Capital
#' @param ebit A number Current year EBIT
#' @param curr_lease_adj A number Lease adjustment to EBIT (lease_adjustment function)
#' @param rnd_adj A number R&D adjustment to EBIT
#' @param eff_tax A number Effective tax rate (Tax Paid / Taxable Income)
#' @param short_debt A number Short term debt (Short term interest bearing debt (debt + leases))
#' @param long_debt A number Long term debt (Debt + Leases)
#' @param equity A number Equity
#' @param goodwill A number Goodwill
#' @param cash A number Cash
#' @param minority A number Minority interest (default=0)
#' @param last_lease_debt A number Last year capitalized leases (present value)
#' @param rnd_asset A number The unamortized part of R&D
#' @param goodwill_portion A number Part of goodwill added to Invested capital (default=0.2)
#' @param cash_portion A number Part of cash added to Invested capital (default=0.2)
#' @param current_year_rnd A number Current year R&D spending
#' @param amortized_rnd A number Amortized portion of R&D
#' @return A tibble ROIC and other parameters
#' @examples
#' roic <- get_roic(ebit=2033, curr_lease_adj=56763, eff_tax=0.2, short_debt=333,
#'                  long_debt=4567, equity=4444, ...)
#' @export
get_roic <- function(ebit, curr_lease_adj=0, rnd_adj=0, eff_tax,
                     short_debt, long_debt, equity, goodwill, cash,
                     last_lease_debt=0, rnd_asset=0, goodwill_portion=0.2, cash_portion=0.2,
                     minority=0) {
  # Portion of goodwill and cash to include in Invested capital
  goodwill_back = goodwill*goodwill_portion
  cash_back = cash*cash_portion
  goodwill_adj = goodwill*(1-goodwill_portion)
  cash_adj = cash*(1-cash_portion)
  # Calculate adjusted EBIT (leases, R&D)
  adj_ebit <- ebit + curr_lease_adj + rnd_adj
  # Calculate after-tax EBIT
  after_tax_ebit <- adj_ebit*(1-eff_tax) + rnd_adj*eff_tax
  # Calculate invested capital
  invested_capital <- short_debt+long_debt+equity+minority-goodwill_adj-cash_adj
  # Calculate adjusted invested capital
  adj_invested_capital <- invested_capital+last_lease_debt+rnd_asset+goodwill_back+cash_back
  # Calculate ROIC
  roic <- (ebit*(1-eff_tax))/invested_capital
  # Calculate adjusted ROIC
  adj_roic <- after_tax_ebit/adj_invested_capital
  # Return ROIC
  return <- tibble::tibble(ebit=ebit, adj_ebit=adj_ebit, invested_capital=invested_capital,
                           after_tax_ebit=after_tax_ebit,
                           adj_invested_capital=adj_invested_capital, roic=round(roic*100, 1),
                           adj_roic=round(adj_roic*100, 1), goodwill_back=goodwill_back,
                           cash_back=cash_back)
}

#' Calculate ROE
#' @description Calculate Return on Equity
#' @param income A number Current year income
#' @param rnd_adj A number R&D adjustment
#' @param equity A number Current year Equity
#' @param goodwill A number Goodwill
#' @param goodwill_portion A number Portion of goodwill adjustment (default=0)
#' @param rnd_asset A number Unamortized portion of R&D
#' @return A tibble ROE and the other parameters
#' @examples
#' ROE <- get_roe(income=333, rnd_adj=555, goodwill=5555)
#' @export
get_roe <- function(income, rnd_adj, equity, goodwill, goodwill_portion=1, rnd_asset) {
  # Calculate goodwill
  goodwill_back = goodwill*goodwill_portion
  goodwill_adj = goodwill*(1-goodwill_portion)
  # Calculate adjusted net income
  adj_income <- income + rnd_adj
  # Calculate book value of invested equity
  book_equity <- equity - goodwill_adj
  # Calculate adjusted book value of equity
  adj_equity <- book_equity + rnd_asset + goodwill_back
  return <- tibble::tibble(income=income, adj_income=adj_income,
                           equity=equity, adj_equity=adj_equity,
                           roe=(income/book_equity)*100,
                           adj_roe=(adj_income/adj_equity)*100,
                           rnd_adj=rnd_adj, goodwill_back=goodwill_back,
                           goodwill_adj=goodwill_adj)
}
