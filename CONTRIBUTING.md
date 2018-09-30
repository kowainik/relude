# Contributing to the Kowainik repositories

## :wave: Greetings Traveler!

We are delighted you're reading this, and we appreciate the effort you're
taking to make our projects awesome! :sparkles:

## How to contribute

### :bug: Report bugs or feature request :bulb:

If you discover a bug or have any proposals on how to make this project better
don't hesitate to create an issue [here](../../issues/new) in a free format.

### Create a PR

We love to receive pull requests from everyone! It's usually a good idea
to tell about your intention to work on something under the corresponding
issue, so everyone is aware that you're on it. If there's no such issue — simply
create a new one!

To get started with the Pull Request implementation you should first 
[fork](../../fork), then clone the repo:

    git clone git@github.com:your-username/project-name.git

Make your changes and consider the following checklist to go through 
before submitting your pull request.

### :white_check_mark: Check list
- [ ] New/fixed features work as expected (Bonus points for the new tests).
- [ ] There are no warnings during compilation.
- [ ] `hlint .` output is: _No Hints_ (see [`hlint`][hlint] tool docs).
- [ ] The code is formatted with the [`stylish-haskell`][stylish-tool] tool 
      using [stylish-haskell.yaml][stylish] file in the repository.
- [ ] The code style of the files you changed is preserved (for more specific 
      details on our style guide check [this document][style-guide]).
- [ ] Commit messages are in the proper format.
      Start the first line of the commit with the issue number in square parentheses.
      
    **_Example:_** `[#42] Upgrade upper bounds of 'base'`

After all above is done commit and push to your fork.
Now you are ready to [submit a pull request](../../compare).


----------
Thanks for spending your time on reading this contributing guide! :sparkling_heart:

[stylish]: .stylish-haskell.yaml
[stylish-tool]: http://hackage.haskell.org/package/stylish-haskell
[hlint]: http://hackage.haskell.org/package/hlint
[style-guide]: https://github.com/kowainik/org/blob/master/style-guide.md#haskell-style-guide
