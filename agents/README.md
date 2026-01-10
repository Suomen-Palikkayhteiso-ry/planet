# Repository Orientation Index for Agents

## 1. What This System Is

This repository contains the source code for `planet`, a Haskell CLI tool that aggregates RSS/Atom feeds into a static, single-page HTML overview. Additionally, it includes an interactive Elm-based web viewer for displaying the aggregated content in a modern, responsive interface.

## 2. What This System Is NOT

- It is **not** a dynamic web server.
- It is **not** a content management system (CMS).
- It does **not** have a database.
- It does **not** handle user accounts or interactions.

## 3. Core Invariants

- **Single-File Output**: The primary output MUST be a single, self-contained `public/index.html` file with embedded CSS and JS.
- **Configuration Driven**: All content and primary settings (title, feeds, locale) MUST be driven by the `planet.toml` file.
- **Stateless Execution**: Each run is independent and generates the site from scratch.

## 4. How to Get Started (Agent Reading Order)

1.  **Start Here**: Begin with `AGENTS.md` to understand the rules of engagement and the project's structure from an agent's perspective.
2.  **Understand the Architecture**: Review the ADRs in `agents/adrs/` to grasp the key architectural decisions and constraints.
3.  **Understand the "Why"**: Read the user stories in `agents/stories/` to understand the intended features and user-facing behavior.
4.  **Understand the "What"**: Examine the tests in `test/` (especially `test/Spec.hs`) to see the concrete, verifiable behaviors that are considered correct.
    - For the Elm application, review `elm-app/tests/` for frontend-specific tests.
5.  **Understand the "How"**: Finally, read the implementation in `src/`.
    - `src/Planet.hs`: The main orchestration logic.
    - `src/FeedParser.hs`: Handles `planet.toml` parsing and feed data extraction.
    - `src/HtmlGen.hs`: Constructs the final HTML output.
    - `src/ElmGen.hs`: Generates Elm data modules from parsed feeds.
    - For the Elm application: `elm-app/src/Main.elm`, `elm-app/src/Types.elm`, `elm-app/src/DateUtils.elm`, `elm-app/src/View.elm`.

**Remember the Precedence:** Tests > ADRs > User Stories > Implementation Comments.
