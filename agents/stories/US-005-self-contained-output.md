# US-005: Self-Contained Output

**As a user,** I want the output to be a single, self-contained HTML file that I can easily host, with no external CSS or JavaScript files.

## Acceptance Criteria

- The final output must be a single `index.html` file.
- All required CSS styles must be embedded within a `<style>` tag in the HTML's `<head>`.
- All required JavaScript must be embedded within a `<script>` tag, if any is used.

## Related Artifacts

- **Implementation:**
  - `src/HtmlGen.hs` (embeds styles and scripts)
  - `src/Styles.hs` (defines CSS)
  - `src/Scripts.hs` (defines JS)
- **ADRs:** `ADR-0000-agent-guidance.md`
