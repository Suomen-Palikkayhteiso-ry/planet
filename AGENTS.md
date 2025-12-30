# Planet Generator - Agent Context

## Project Mission
`planet` is a Haskell CLI tool that aggregates RSS/Atom feeds (blogs and YouTube channels) into a static, single-page HTML overview. It is designed to be lightweight, self-contained, and easily deployable via GitHub Pages.

## Architecture
- **Type**: CLI Application
- **Language**: Haskell (GHC 9.6)
- **Entry Point**: `src/Main.hs` (Contains all business logic)
- **Configuration**: `planet.toml` (TOML format)
- **Output**: `public/index.html` (Static HTML with embedded CSS)

## Key Libraries & Decisions
- **`htoml-megaparsec`**: Used for TOML parsing. Note: We use `unordered-containers` (HashMap) and `vector` for intermediate TOML structures (`VTArray`, `VTable`).
- **`feed`**: For parsing abstract RSS/Atom feeds.
- **`http-conduit`**: For robust HTTP fetching.
- **`async`**: Used to fetch multiple feeds concurrently (`mapConcurrently`) to speed up generation.
- **`blaze-html`**: HTML DSL for type-safe view generation.
- **Embedded CSS**: CSS is defined as a multiline string in `Main.hs` to keep the output strictly single-file.

## Development Environment
The project uses `devenv` (Nix-based) to ensure a reproducible environment.

### Setup & Run
1.  **Enter Shell**: `devenv shell`
2.  **Run**: `cabal run`
    *   This compiles the project and executes the binary.
    *   Reads `planet.toml`.
    *   Writes to `public/index.html`.

### Configuration (`planet.toml`)
Structure:
```toml
title = "My Planet"

[[feeds]]
type = "blog" # or "youtube"
title = "Feed Title"
url = "https://example.com/rss.xml"
```

## Maintenance Notes
- **GHC Version**: Pinned to 9.6 in `devenv.nix`.
- **CI/CD**: `.github/workflows/deploy.yml` handles building and deployment to GitHub Pages. It uses `cabal` inside `devenv`.
- **Dependencies**: Managed via `planet.cabal`. If adding new deps, ensure they are added there and `cabal build` resolves them.

## Common Tasks
- **Adding a new feed**: Edit `planet.toml`.
- **Changing styling**: Edit the `css` function in `src/Main.hs`.
- **Modifying logic**: All logic resides in `src/Main.hs`.
