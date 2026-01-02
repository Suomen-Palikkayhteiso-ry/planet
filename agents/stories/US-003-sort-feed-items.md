# US-003: Sort Feed Items

**As a user,** I want to see all feed items sorted by their publication date, with the newest items appearing first.

## Acceptance Criteria

- After fetching all items from all feeds, the application must sort the combined list.
- The sorting key must be the publication date/time of each item.
- The sorting order must be descending (newest to oldest).

## Related Artifacts

- **Implementation:** `src/Planet.hs` (`sortedItems = sortOn (\i -> Down (itemDate i)) allItems`)
- **ADRs:** `ADR-0000-agent-guidance.md`
