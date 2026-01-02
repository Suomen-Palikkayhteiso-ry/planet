# US-001: Aggregate Feeds

**As a user,** I want to aggregate multiple RSS/Atom feeds into a single HTML page.

## Acceptance Criteria

- The application must read a list of feed URLs from a configuration source.
- The application must fetch content from each URL.
- The application must combine the items from all feeds into one collection.
- The final output must be a single HTML file.

## Related Artifacts

- **Tests:** `test/Spec.hs` (configTests, htmlTests)
- **ADRs:** `ADR-0000-agent-guidance.md`
