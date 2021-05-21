library(fun.valuation)

# Test rnd_adjustment
test_that("Verify R&D adjustment to EBIT", {
  current_rnd <- 1771
  amortized_rnd <- 1409

  expect_equal(rnd_adjustment(current_rnd = current_rnd,
                              amortized_rnd = amortized_rnd), 362)
})

# Test get_amortization_2 function
test_that("Verify amortized and amortized R&D (2Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714)

  # Test amortized value
  expect_equal(get_amortization_2(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 8878)
  # Test unamortized value
  expect_equal(get_amortization_2(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 14332)
})

# Test get_amortization_3 function
test_that("Verify amortized and amortized R&D (3Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010)

  # Test amortized value
  expect_equal(get_amortization_3(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 8922)
  # Test unamortized value
  expect_equal(get_amortization_3(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 18744)
})


# Test get_amortization_5 function
test_that("Verify amortized and amortized R&D (5Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010, y4=6500, y5=4000)

  # Test amortized value
  expect_equal(get_amortization_5(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 7453)
  # Test unamortized value
  expect_equal(get_amortization_5(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 27177)
})


# Test get_amortization_10 function
test_that("Verify amortized and amortized R&D (10Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010, y4=6500, y5=4000,
                     y6=4500, y7=3456, y8=9836, y9=23465, y10=4567)

  # Test amortized value
  expect_equal(get_amortization_10(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 8309)
  # Test unamortized value
  expect_equal(get_amortization_10(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 44278)
})

# Test get_leases function
test_that("Verify capitalized leases value and embedded years", {
  # Paid leases
  leases <- list(y1=471, y2=378, y3=295, y4=225, y5=163)

  # Leases paid beyond threshold
  beyond <- 791
  beyond_2 <- 310
  null_beyond <- 0
  # Cost of debt
  cost_debt <- 0.024
  # Test capitalized leases
  expect_equal(get_leases(lease_flow = leases,
                          beyond=beyond,
                          cost_debt = cost_debt)$debt_value_lease,
               2114)

  # Test embedded years
  expect_equal(get_leases(lease_flow = leases,
                          beyond=beyond,
                          cost_debt = cost_debt)$years_embedded,
               3)

  # Test capitalized leases, beyond mean(leases) = 1
  expect_equal(get_leases(lease_flow = leases,
                          beyond = beyond_2,
                          cost_debt = cost_debt)$debt_value_lease,
               1713)

  # Test embedded years, beyond mean(leases) = 1
  expect_equal(get_leases(lease_flow = leases,
                          beyond = beyond_2,
                          cost_debt = cost_debt)$years_embedded,
               1)

  # Test capitalized leases, beyond = 0
  expect_equal(get_leases(lease_flow = leases,
                          beyond = null_beyond,
                          cost_debt = cost_debt)$debt_value_lease,
               1444)

  # Test embedded years, beyond = 0
  expect_equal(get_leases(lease_flow = leases,
                          beyond = null_beyond,
                          cost_debt = cost_debt)$years_embedded,
               0)

})

# Test lease_adjustment function
test_that("Verify lease adjustment to EBIT", {
  current_year_leases <- 863
  lease_debt_value <- 2114
  lease_embedded_years <- 3
  expect_equal(lease_adjustment(current_lease = current_year_leases,
                                debt_value_lease = lease_debt_value,
                                lease_embedded_years = lease_embedded_years),
               598)
})

# Test get_roic function
test_that("Verify ROIC calculation", {
  ebit <- 27801
  lease_adj <- 1286
  rnd_adj <- 361
  tax_rate <- 0.31
  short_debt <- 6348
  long_debt <- 47079
  equity <- 71315
  minority <- 4446
  goodwill <- 16520
  cash <- 6550
  last_year_lease <- 14134
  # Unamortized portion R&D
  rnd_asset <- 4831
  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = rnd_adj, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = rnd_asset,
                        minority = minority)$roic), 17)

  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = rnd_adj, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = rnd_asset,
                        minority = minority)$adj_roic), 15)

  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = 0, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = 0,
                        minority = minority)$roic), 17)

  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = 0, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = 0,
                        minority = minority)$adj_roic), 15)

})


# Test get_roe function
test_that("Verify ROE calculation", {
  net_income <- 16999
  rnd_adj <- 361
  tax_rate <- 0.31
  equity <- 71315
  goodwill <- 16521
  # Unamortized portion R&D
  rnd_asset <- 4831
  expect_equal(get_roe(income = net_income, rnd_adj = rnd_adj, equity = equity,
                       goodwill_portion = 0.2, rnd_asset = rnd_asset,
                       eff_tax = tax_rate, goodwill = goodwill)$roe, 27)

  expect_equal(get_roe(income = net_income, rnd_adj = rnd_adj, equity = equity,
                       goodwill_portion = 0.2, rnd_asset = rnd_asset,
                       eff_tax = tax_rate, goodwill = goodwill)$adj_roe, 25)

  expect_equal(get_roe(income = net_income, rnd_adj = 0, equity = equity,
                       goodwill_portion = 0.2, rnd_asset = 0,
                       eff_tax = tax_rate, goodwill = goodwill)$roe, 29)

})

# Calculate bottom-up beta
test_that("Verify bottom-up beta", {
  average_beta <- 0.77
  industry_tax <- 0.0597
  average_de <- 0.083
  firm_tax <- 0.16
  firm_debt <- 13.03
  firm_equity <- 212.92
  expect_equal(get_beta(average_beta = average_beta,
                        industry_tax = industry_tax,
                        average_de = average_de,
                        firm_tax = firm_tax,
                        firm_debt = firm_debt,
                        firm_equity = firm_equity)$unlevered_beta, 0.71)
  expect_equal(get_beta(average_beta = average_beta,
                        industry_tax = industry_tax,
                        average_de = average_de,
                        firm_tax = firm_tax,
                        firm_debt = firm_debt,
                        firm_equity = firm_equity)$levered_beta, 0.75)
})

# Calculate Cost of Equity
test_that("Verift cost of equity", {
  risk_free <- 0.015
  beta <- 0.7
  risk_premium <- 0.041
  expect_equal(get_cost_equity(risk_free = risk_free,
                               beta = beta,
                               risk_premium = risk_premium)$cost_equity, 0.044)
})

# Calculate Cost of Debt
test_that("Verify cost of equity", {
  risk_free <- 0.015
  company_spread <- 0.01
  expect_equal(get_cost_debt(risk_free = risk_free,
                               company_spread = company_spread)$cost_debt, 0.025)
})

# Calculate Cost of Capital
test_that("Verify cost of capital", {
    marginal_tax <- 0.35
    cost_equity <- 0.0875
    cost_debt <- 0.0293
    equity <- 340
    debt <- 600
    expect_equal(get_cost_capital(marginal_tax = marginal_tax,
                                  cost_equity = cost_equity,
                                  cost_debt = cost_debt,
                                  equity = equity,
                                  debt = debt)$cost_capital, 0.0438)
})

# Test get_acquisition function
test_that("Verify amortized and amortized acquisition (5Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010, y4=6500, y5=4000)
  # Test amortized value
  expect_equal(get_acquisition(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 7453)
  # Test unamortized value
  expect_equal(get_acquisition(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 27177)
})

# Test get_net_capex
test_that("Verify net CapEx", {
  capex <- 584
  depreciation <- 486
  rnd_expense <- 1594
  rnd_amortization <- 485
  acquisition <- 2516
  expect_equal(get_net_capex(capex = capex, depreciation = depreciation,
                             rnd_expense = rnd_expense, rnd_amortization = rnd_amortization,
                             acquisition = acquisition)$net_capex, 3723)
})

# Test get_working_cap
test_that("Verify working capital", {
  inventory <- 50
  accounts_receive <- 50
  accounts_payable <- 60
  expect_equal(get_working_cap(inventory = inventory, accounts_receive = accounts_receive,
                               accounts_payable = accounts_payable)$working_capital, 40)
  expect_equal(get_working_cap(inventory = inventory, accounts_receive = accounts_receive,
                               accounts_payable = 200)$working_capital, 0)
})

# Test get_rr
test_that("Verify reinvestment rate", {
  after_tax_ebit <- 200
  working_capital <- 50
  net_capex <- 50
  expect_equal(get_rr(net_capex = net_capex, working_capital = working_capital,
                      after_tax_ebit = after_tax_ebit)$reinvestment_rate, 0.5)
})

# Test get_stable_growth
test_that("Verify stable growth", {
  reinvestment_rate <- 0.2
  roic <- 0.15
  expect_equal(get_stable_growth(reinvestment_rate = reinvestment_rate, roic = roic)$stable_growth,
               0.03)
})

# Test get_dynamic_growth
test_that("Verify dynamic growth", {
  reinvestment_rate <-  0.5299
  roic_initial <-  0.1218
  roic_target <-  0.1722
  years_target <-  5
  expect_equal(get_dynamic_growth(reinvestment_rate = reinvestment_rate,
                                  roic_initial = roic_initial,
                                  roic_target = roic_target,
                                  years_target = years_target)$growth, 0.16)
})

# Test get_fcff
test_that("Verify free cash flow to firm", {
  after_tax_ebit <- 2481
  net_capex <- 800
  working_capital <- 500
  wacc <- 0.1205
  growth <- 0.0546
  cash <- 111
  debt <- 475
  current_share_price <- 34
  share_out <- 111
  expect_equal(get_stable_operating_assets(net_capex = net_capex,
                        working_capital = working_capital,
                        after_tax_ebit = after_tax_ebit,
                        growth = growth,
                        wacc = wacc,
                        cash = cash,
                        debt = debt,
                        current_share_price = current_share_price,
                        share_out = share_out)$fcff, 1181)
  expect_equal(get_stable_operating_assets(net_capex = net_capex,
                        working_capital = working_capital,
                        after_tax_ebit = after_tax_ebit,
                        growth = growth,
                        wacc = wacc,
                        cash = cash,
                        debt = debt,
                        current_share_price = current_share_price,
                        share_out = share_out)$value_operating_assets, 18900)
})

# Test get_operating_assets
test_that("Verify value of operating assests", {
  npv_fcff <- 9733
  wacc <- 0.0674
  terminal_value <- 65597
  cash <- 111
  debt <- 475
  current_share_price <- 34
  share_out <- 111
  expect_equal(get_operating_assets(npv_fcff=npv_fcff,
                                    wacc=wacc,
                                    terminal_value = terminal_value,
                                    cash = cash,
                                    debt = debt,
                                    current_share_price = current_share_price,
                                    share_out = share_out)$value_operating_assets,
               57075)
})

# Test get_terminal_value
test_that("Verify terminal value", {
  after_tax_ebit <- 4289
  stable_growth <- 0.03
  roic <- 0.0674
  wacc <- 0.0674
  expect_equal(get_terminal_value(ebit_year_five = after_tax_ebit,
                                  stable_growth = stable_growth,
                                  roic = roic,
                                  wacc = wacc)$terminal_value, 65549)
})

# Test get_cash_flow
test_that("Verify NPV", {
  after_tax_ebit <- 3474.9
  reinvestment_rate <- 0.4
  time_period <- c(1,2,3,4,5)
  wacc <- 0.0674
  growth <- 0.043
  expect_equal(get_cash_flow(after_tax_ebit = after_tax_ebit,
                             reinvestment_rate = reinvestment_rate,
                             time_period = time_period,
                             wacc = wacc,
                             growth = growth)$fcff_npv, 9731)
})

# Test get_nol
test_that("Verify NOL", {
  nol_1 <- 400
  nol_2 <- 200
  ebit <- 300
  expect_equal(get_nol(nol=nol_1, ebit = ebit), 100)
  expect_equal(get_nol(nol=nol_2, ebit = ebit), 0)
})

# Test get_growth_taxes
test_that("Verify paid taxes", {
  ebit_1 <- -50
  ebit_2 <- 300
  ebit_3 <- 500
  last_nol <- 400
  marginal_tax <- 0.35
  expect_equal(get_growth_taxes(ebit = ebit_1,
                                last_nol = last_nol,
                                marginal_tax = marginal_tax), 0)

  expect_equal(get_growth_taxes(ebit = ebit_2,
                                last_nol = last_nol,
                                marginal_tax = marginal_tax), 0)

  expect_equal(get_growth_taxes(ebit = ebit_3,
                                last_nol = last_nol,
                                marginal_tax = marginal_tax), 35)
})

# Test get_beta_start_up
test_that("Verify beta, cost of capital etc.", {
  beta<-1.5
  risk_free <- 0.035
  risk_premium <- 0.05
  debt <- 109.65
  shares_out <- 95.63
  stock_price <- 29
  cost_debt <- 0.08
  expect_equal(get_beta_start_up(beta = beta,
                                 risk_free = risk_free,
                                 risk_premium = risk_premium,
                                 debt = debt,
                                 shares_out = shares_out,
                                 stock_price = stock_price,
                                 cost_debt = cost_debt)$cost_capital, 0.11)
})

# Test get_tv
test_that("Verify start-up terminal value", {
  terminal_nol <- 0
  terminal_fcff <- 585.7475
  cost_capital <- 0.067
  marginal_tax <- 0.037
  terminal_growth <- 0.03
  expect_equal(get_tv(terminal_nol = terminal_nol,
                      terminal_fcff = terminal_fcff,
                      cost_capital = cost_capital,
                      marginal_tax = marginal_tax,
                      terminal_growth = terminal_growth), 15831)
  expect_equal(get_tv(terminal_nol = 100,
                      terminal_fcff = terminal_fcff,
                      cost_capital = cost_capital,
                      marginal_tax = marginal_tax,
                      terminal_growth = terminal_growth), 15835)
})
