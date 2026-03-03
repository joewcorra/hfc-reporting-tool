# ADR-001: Partition HFC Consumption by Application Before Emissions Calculations

**Date:** 2026-02-28  
**Status:** Accepted  
**Authors:** Joe [+ Claude]

---

## Context

The HFC Emissions Tool estimates greenhouse gas emissions by application (refrigeration and air conditioning, foams, aerosols, etc.) using Kigali Amendment consumption data as its primary input. Kigali Protocol reporting requires countries to report aggregate HFC consumption by gas across all applications — compilers do not report application-level breakdowns.

To bridge this gap, the tool uses consumption allocation proportions derived from Velders et al. (2015) to partition aggregate consumption across applications. The question is: **at what point in the calculation pipeline should this partitioning occur?**

An earlier implementation applied Velders proportions *after* running the IPCC emissions calculations, multiplying total emissions by application shares as a post-processing step.

---

## Decision

Velders partitioning is applied **before** any emissions calculations. The pipeline is structured as:

1. Kigali data in → mixture decomposition → net consumption by `(year, component, flow)`
2. Apply Velders proportions → consumption by `(year, component, application, flow)`
3. Join application-specific default parameters (equipment lifetime, leak rates, etc.)
4. Run IPCC methodology calculations separately for each `(component × application)` slice

---

## Rationale

Different applications have materially different physical characteristics that affect emissions estimates:

- **Equipment lifetime** — domestic refrigerators have much longer lifetimes than mobile AC units, which directly determines how much refrigerant is in the bank at end-of-life and when end-of-life emissions occur
- **Leak rates (emission factor, installed base)** — leak rates vary substantially across applications
- **Destruction/reclaim rates at end of life** — recovery infrastructure differs by sector

The previous approach of partitioning *after* calculations implicitly assumed uniform parameters across all applications, then scaled the resulting emissions proportionally. This is methodologically incorrect: the bank model is nonlinear and time-dependent, so applying a share to the *output* is not equivalent to running the model on the *input* scaled by that share.

Partitioning first ensures each application slice uses the correct parameters before any calculations run, producing emissions estimates that are application-appropriate rather than proportionally rescaled from a single aggregate.

---

## Consequences

**Positive:**
- Methodologically correct — each application slice uses its own parameters
- Ready to accept application-specific default parameters without pipeline changes (see Deferred below)
- The `process_all_components()` function now groups by `(component, application)` rather than `component` alone, making the grouping logic explicit and easy to extend

**Negative / Trade-offs:**
- The number of calculation slices increases by a factor of N applications, increasing compute time modestly. Not expected to be significant at current data volumes.
- The Kigali data entry table no longer includes an application column, which is the correct behaviour (compilers genuinely don't know the application breakdown) but required removing UI elements and updating downstream column references

---

## Deferred / Known Limitations

**Uniform Velders shares across HFCs:** Currently the same application proportions (derived from EDGAR scoping data via Velders) are applied to every HFC component. In reality, different HFCs are used predominantly in different applications (e.g. HFC-134a is heavily weighted toward MAC; HFC-245fa is almost exclusively foam). HFC-specific Velders allocation proportions should be incorporated in a future phase. The code includes a comment flagging this location.

**Uniform default parameters across applications:** The `hfc_defaults` table currently contains one row per HFC with identical parameter values across all applications. When application-specific parameters become available, they should be added as additional rows keyed by `(hfc, application)`, and the join in the `emissions_data` reactive should be updated to include `meta_application` as a join key. No other pipeline changes will be needed.

---

## Alternatives Considered

**Post-hoc partitioning (previous approach):** Apply Velders shares to emissions outputs rather than consumption inputs. Rejected because it assumes linearity in the bank model and prevents application-specific parameters from being used correctly.

**User-entered application breakdown:** Require compilers to enter consumption by application directly. Rejected because this data is not available to Kigali Protocol reporters; the entire purpose of the Velders allocation step is to estimate these proportions from research data.

---

## References

- Velders, G.J.M. et al. (2015). Preserving Montreal Protocol climate benefits by limiting HFCs. *Science*, 351(6275), 922–923.
- IPCC (2006). 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 3, Chapter 7: Emissions of Fluorinated Substitutes for Ozone Depleting Substances.
- Kigali Amendment to the Montreal Protocol (2016).
