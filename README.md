# KoWePS JEBO replication package

Local replication package for the November 3, 2026 draft, *Income Shortfalls Relative to Recent Living Standards and Depressive Symptoms: Evidence from a Korean Panel*. The draft is in `reference/`.

## Run

From this folder, run:

```sh
Rscript run_all.R
Rscript code/event_placebo_fixed_sample_20261030.R
```

The first command rebuilds the corrected tables, event dynamics and two figures in `results/`. The second reruns the CES-D paired-event checks for Table 4 Panel B and writes to `results/paired/`. Both use the prepared local inputs in `inputs/`. These include restricted KoWePS-derived records and should be kept local. Run logs and R session information are included.

Verified with R 4.5.2, data.table 1.18.6.1, fixest 0.14.2, did 2.3.0, ggplot2 4.0.2 and DRDID 1.2.3.

## Verification and coverage

- All 25 table CSVs, both figure-data CSVs, and both PNGs rebuilt by the main runner matched their October 31 counterparts byte for byte.
- The paired CES-D results matched the saved fixed-sample CSV byte for byte. The complete-window event-zero estimate is 0.0386 with 1,640 exposed adults, as shown to three decimals in Table 4 Panel B.
- The November draft's embedded Figure 2 matches the generated Figure 2 byte for byte. Its embedded Figure 1 differs from the October build and is included as `reference/figures/manuscript_figure1.png`.
- `reference/supplemental/` includes the saved Appendix A13 inputs for event types, rebuilt definitions, HonestDiD bounds and selection. The runners use prepared data and cover the main October workflow and paired-event checks. Raw survey reconstruction, Appendix A13 regeneration, and the November Word build are outside the verified run. The saved `reference/tables/T3_annual.csv` is also included for reference.
- See `docs/CHANGE_LOG.md` for the October correction history and exhibit map.
