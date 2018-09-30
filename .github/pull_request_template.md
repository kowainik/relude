Resolves #{PUT_ISSUE_NUMBER_HERE}

<!-- You can add any comments here -->

## Checklist:

<!--- Go over all the following points, and put an `x` in all the boxes that apply. -->
<!--- If you're unsure about any of these, don't hesitate to ask. We're here to help! -->

### HLint

- [ ] I've changed the exposed interface (add new reexports, remove reexports, rename reexported things, etc.).
  - [ ] I've updated [`hlint.dhall`](https://github.com/kowainik/relude/blob/master/hlint/hlint.dhall)
        accordingly to my changes (add new rules for the new imports, remove old ones, when they are outdated, etc.).
  - [ ] I've generated the new `.hlint.yaml` file (see 
        [this instructions](https://github.com/kowainik/relude#generating-hlintyaml)).

### General

- [ ] I've updated the [CHANGELOG](https://github.com/kowainik/relude/blob/master/CHANGELOG.md) 
      with the short description of my latest changes.
- [ ] All new and existing tests pass.
- [ ] I keep the code style used in the files I've changed (see 
      [style-guide](https://github.com/kowainik/org/blob/master/style-guide.md#haskell-style-guide) 
      for more details).
- [ ] I've used the [`stylish-haskell` file](https://github.com/kowainik/relude/blob/master/.stylish-haskell.yaml).
- [ ] My change requires the documentation updates.
  - [ ] I've updated the documentation accordingly.
- [ ] I've added the `[ci skip]` text to the docs-only related commit's name.
