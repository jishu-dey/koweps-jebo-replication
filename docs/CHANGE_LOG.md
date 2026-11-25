# Change log: corrected manuscript

31 October 2026. File: `KOWEPS_JEBO_Aug22_corrected_2026-10-31.docx` (in Downloads). The original `KOWEPS_JEBO_Aug22 (3).docx` is unchanged. Item numbers refer to `../paper_corrections_2026_10_31/CORRECTIONS.md`. Build: `scripts/paper_corrected_tables_20261031.R` then `scripts/paper_build_corrected_docx_20261031.py`.

## Defaults used (decisions still open to the authors)

- Main income measure: positive real logs, exact calendar histories (N = 148,887), levels as a stated sensitivity.
- Structure: original section order and benchmark framing kept, as an observational study. Figure 2 was not promoted to lead the paper.
- Tables: follow the table-retention audit with its 30 October correction.

## Table map

| New | Old | Change |
|---|---|---|
| Table 1 | Table 1 | Rebuilt on the main sample |
| Table 2 | Table 2 | Rebuilt; symmetry p for CES-D >= 16 added; one inference convention |
| Table 3 | Table 3 | Now a same-sample specification ladder |
| Table 4, Figure 2 | Table 5, Figure 2 | Corrected timing, common base, explicit pre-period tests; placebo and TWFE rows removed |
| Table 5 | Table 8 | Income-control columns, asymmetry tests, BH q, asset groups added |
| Figure 1 | Figure 1 | Redrawn; lines use Table 2 coefficients |
| A1, A2, A3, A4, A5 | same | Rebuilt; A2 relabelled AR(3) with horse race; A3 now income definitions; A5 common base with uniform bands |
| A6 | new | Floor-effect checks |
| A7 | A10 | Correct fixed-effects estimator |
| A8 | A11 | Upside and symmetry p; common-sample windows |
| A9 | A12 | Clustering by household group; region-by-year columns removed (variable unavailable) |
| A10 | A8 | Exact calendar; placebo row removed |
| A11 | Table 4 Panel A | Reverting and persisting shortfalls, with income controls |
| A12 | Table 6 | Both income sides, unrestricted sample, no insurance term |
| A13 | new | Event types, rebuilt definitions, gain placebo, HonestDiD, selection |
| Removed | Table 4 Panels B-C, Table 7, A6, A7, A9, A13 | Per items 26, 27, 31, 30, 55, 56 and the retention audit |

## Status of the 69 audit items

- Applied: items 1-58 and 60-69, subject to the notes below.
- Partly applied: item 59 (package cannot run from raw data; the raw file is missing, and the new scripts run from preserved extracts). Item 46 (attrition is disclosed, not modelled).
- Figures still from the complete-history sample (N = 132,980), labelled as such in the text: holdout RMSE, wild bootstrap, lagged interior-score floor test, floor simulation.

## Authors must confirm before submission

1. `h_soc_13` labels against the KoWePS codebook (the paper now only says fewer than ten records are non-enrolled).
2. Why the analysis starts at wave 7. The text reports first appearances (about 4,360 people) as suggesting a sample addition; that is inferred from the data.
3. Internal results cited without a table: closure/layoff study (about 5 percent income change), policy-shock screening, fixed 2015-2020 sample (0.057), holdout RMSE, wild bootstrap, floor simulation. Add appendix material or cut those sentences.
4. New citations. Reference details were checked against PubMed, RePEc or publisher pages for Wang, Yaman et al., Korbel and Škvrňák, Chen and Roth, and Rambachan and Roth; Nickell (1981) is from memory. Their findings were read from abstracts or earlier notes, not full text.
5. Descriptions of other cited studies in Sections 2.1-2.4 were not re-verified.
6. New prose. This is a substantial rewrite of the abstract, introduction, Sections 3-6 and all notes, drafted for your review. All analyses are exploratory and post hoc, as the paper now says.
7. Self-esteem event path: clean pre-period test p = 0.075 (household) and 0.001 (person). The text says this; consider whether to show the path at all.
8. Data availability statement: left as is. Add a code statement once the package runs from source.
