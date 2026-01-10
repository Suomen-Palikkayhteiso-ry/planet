# US-009: Client-Side Search with Reduced Bundle Size

**As a user,** I want the application to load quickly by embedding only essential data and fetching a search index on demand, so that I can search feed items without a large initial download.

## Acceptance Criteria

- The initial JavaScript bundle does not include full feed item descriptions.
- Only snippets or minimal data are embedded in the bundle.
- A search index is generated on the backend and made available for download.
- The frontend fetches the search index asynchronously after initial load.
- Search functionality works seamlessly after the index is loaded.
- Bundle size is significantly reduced compared to embedding full data.

## Related Artifacts

- **Tests:** `test/FeedParserSpec.hs`, `elm-app/tests/MainTest.elm`
- **ADRs:** `ADR-0000-agent-guidance.md`