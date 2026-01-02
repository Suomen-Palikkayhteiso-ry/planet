# US-002: Configure Planet via TOML

**As a user,** I want to configure the feeds, page title, locale, and timezone using a `planet.toml` file.

## Acceptance Criteria

- The application must read `planet.toml` on startup.
- It must parse the main `title` for the generated page.
- It must parse a `locale` (e.g., "en", "fi") for internationalization.
- It must parse a `timezone` (e.g., "Europe/Helsinki") for displaying dates.
- It must parse a list of `[[feeds]]`, each with a `type`, `title`, and `url`.
- The application should handle missing optional fields gracefully (e.g., use a default title).

## Related Artifacts

- **Tests:** `test/Spec.hs` (configTests)
- **ADRs:** `ADR-0000-agent-guidance.md`
- **Implementation:** `src/FeedParser.hs` (parseConfig)
