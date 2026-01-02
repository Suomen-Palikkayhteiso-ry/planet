# AGENTS.md

## Purpose

This document defines how LLM coding agents must read, navigate, and safely modify the `planet` repository. It is the primary entry point for any agent-based task.

## 1. Source of Truth & Precedence

Your understanding and actions must be guided by the following artifacts, in this strict order of precedence:

1.  **Tests (`test/`)**: Define the verifiable, correct behavior of the application. **Tests are the ultimate source of truth.**
2.  **ADRs (`agents/adrs/`)**: Define the architectural constraints and non-negotiable rules.
3.  **User Stories (`agents/stories/`)**: Explain the "why" behind the features and define product intent.
4.  **Glossary (`agents/GLOSSARY.md`)**: Provides the canonical vocabulary for this project.
5.  **Inline Source Code Comments**: Provide context but are subordinate to the artifacts above.

## 2. Agent Self-Guidance Work Loop

Before making any changes, you **MUST** follow this work loop:

1.  **State Goal**: Clearly state the perceived goal of the task.
2.  **Locate Artifacts**: Identify and read all relevant tests, user stories, and ADRs related to the goal.
3.  **Identify Constraints**: List the specific architectural and behavioral constraints imposed by the artifacts.
5.  **Propose Strategy**: Formulate a minimal change strategy that respects all constraints.
5.  **Formalize Behavior (BDD)**: When adding or updating tests, consider expressing the desired behavior in a Given/When/Then format within the test's comments or the user story itself. Tests are the executable specification of behavior.
6.  **Update/Add Tests**: Before writing implementation code, add or update tests that codify the goal. Ensure they fail as expected.
6.  **Implement**: Write the code to make the new tests pass.
7.  **Verify**: Run all tests and re-evaluate your changes against the user stories and ADRs to ensure compliance.

## 3. Repository Map (Agent-Oriented)

- **`./AGENTS.md`**: **Your entry point.** Defines rules of engagement.
- **`./agents/README.md`**: Quick repository orientation (what it is, what it isn't).
- **`./agents/GLOSSARY.md`**: Canonical terminology. Use it to speak the project's language.
- **`./agents/adrs/`**: Architectural Decision Records (ADRs). These are your constraints.
- **`./agents/stories/`**: User Stories. This is the "why."
- **`./test/`**: Behavioral truth.
    - `test/Spec.hs`: Key unit and integration tests.
    - `test/TestSuite.hs`: Test suite entry point.
- **`./src/`**: Implementation.
    - `src/Planet.hs`: Main business logic orchestration.
    - `src/FeedParser.hs`: Configuration and feed data parsing.
    - `src/HtmlGen.hs`: HTML generation logic.
    - `src/Styles.hs` & `src/Scripts.hs`: Embedded CSS and JS.
    - `src/I18n.hs`: Internationalization logic.
- **`./planet.cabal`**: Project definition and dependencies.
- **`./planet.toml`**: Main configuration file.

## 4. Change Rules

- **Do not** modify application behavior without first adding or modifying a test in `test/`.
- **Do not** violate a constraint defined in an ADR. If a change requires this, you must first propose a new ADR.
- All code changes must be accompanied by corresponding updates to tests and, if necessary, documentation.
- All commit messages **MUST** follow the Conventional Commits specification outlined in `agents/adrs/ADR-0000-agent-guidance.md`.

## 5. Decision Escalation Rules

You **MUST STOP** and escalate to the user for guidance if you encounter any of the following situations:

- A requirement in a User Story conflicts with an existing test.
- A proposed change would violate a constraint in an ADR.
- The desired behavior is ambiguous, or there are multiple plausible interpretations.
- You are uncertain how to proceed.

**Escalation Procedure:**
1.  Clearly document the conflict or ambiguity.
2.  If possible, create a new failing test case that demonstrates the ambiguity.
3.  Present the situation to the user and ask for clarification. **Do not guess.**
