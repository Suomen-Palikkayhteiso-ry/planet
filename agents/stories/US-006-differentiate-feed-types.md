# US-006: Differentiate Feed Types

**As a user,** I want to visually distinguish between different types of feeds, such as blogs, YouTube channels, and Flickr galleries.

## Acceptance Criteria

- The `planet.toml` configuration allows specifying a `type` for each feed (e.g., `blog`, `youtube`, `flickr`).
- The generated HTML should use different styling or iconography for items from different feed types.
- The card for each item should clearly indicate its source type.

## Related Artifacts

- **Configuration:** `planet.toml`
- **Implementation:**
  - `src/FeedParser.hs` (parses `FeedType`)
  - `src/HtmlGen.hs` (applies different classes/styles based on `itemType`)
- **ADRs:** `ADR-0000-agent-guidance.md`
