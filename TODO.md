# TODO

Complete as many [ ] task items as possible. After completing each task, test, commit, and push your changes. Mark each completed task as [x] in this TODO.md, including the commit hash in parentheses. If a task is too large to complete at once, create additional TODO files starting with "TODO-" and reference them from this file. Do not commit any TODO files.

Follow the guidelines in AGENTS.md and especially ADR-0000. Do not create any user- or developer-facing documentation. You may maintain documentation for LLM agents by keeping AGENTS.md up to date, concise, and primarily referencing more detailed documentation in ./agents or ./agents/adrs (for Architecture Decision Records).

## Tasks

* [x] Rename "blog" type parsing (planet.toml) to "rss", add alias "default" to it and make it the default type when now type is being defined; make type optional in planet.toml (79e1f9ba1a78bc95f9386f1b6cf336ac078a5bdf)

* [x] Add new "atom" type parsing and make it the same as "flickr", but without removing the first p tag. Therefore, removing the first p tag is "atom" + "flickr" special rules (currently only the first p tag) (79e1f9ba1a78bc95f9386f1b6cf336ac078a5bdf)

* [x] Review "youtube" type parsing implementation; Is is the same as "rss" but with a few extra rules for e.g. to support link rel=alternate? (79e1f9ba1a78bc95f9386f1b6cf336ac078a5bdf)
    * No, "youtube" is not the same as "rss". It has its own media description extraction logic. The "few extra rules" are not just for `link rel=alternate`, but a whole different way of getting the description.

* [x] Review the resulting code base for ADR-0000 conformance and refactoring possibilities for better LLM coding agent based maintainability; Proceed with the changes. (79e1f9ba1a78bc95f9386f1b6cf336ac078a5bdf)
    * Updated `GLOSSARY.md` to include the new `rss` and `atom` feed types.
    * Refactored `FeedParser.hs` to remove a confusing special case for `Rss` feeds.