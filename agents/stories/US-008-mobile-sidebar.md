# US-008: Mobile Sidebar for Timeline and Filters

**As a mobile user,** I want a toggleable sidebar that contains both the timeline navigation and filter/search controls so that I can easily access these features on small screens without cluttering the main view.

## Acceptance Criteria

- On mobile devices, the timeline and filter/search controls are hidden by default.
- A toggle button is visible to open/close the sidebar.
- When the sidebar is open, it overlays the main content or pushes it aside.
- The sidebar contains both timeline navigation and filter/search inputs.
- Tapping outside the sidebar or a close button closes it.
- The sidebar is responsive and works well on various mobile screen sizes.

## Related Artifacts

- **Tests:** `elm-app/tests/ViewTest.elm`, `elm-app/tests/MainTest.elm`
- **ADRs:** `ADR-0000-agent-guidance.md`