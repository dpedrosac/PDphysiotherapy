# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/)
and this project adheres to [Semantic Versioning](https://semver.org/).

---

## [1.2.0] - 2026-02-11
### Changed
- Standardized script headers across the analysis pipeline.
- Refactored and tidied `tidyup_dataframe.R` (readability + safer transformations).
- Improved scale score generation and TableOne export logic.

### Fixed
- Reduced risk of hidden state changes by consolidating transformations into pipelines.
- Added missing-column tolerance in factor conversion helpers.

---

## [1.1.0] - 2025-08-04
### Added
- Additional functionality in data tidying and code debugging improvements.

---

## [1.0.0] - 2025-02-17
### Added
- Initial commit: rename and refactor survey data into analysis-ready format.

