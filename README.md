# Configuration files

## Installation

At some point I should make a script to deploy this configuration, but for now I just perform the below steps.

Initialize git repository in `$HOME`.

```sh
cd ~
git init
```

Add this repository as  a remote.

```sh
git remote add origin 'git@github.com:bkhl/dotfiles.git'
```

Make `status` ignore untracked files. This way you won't get a huge list of your regular files in the status commands. Some people may prefer a `.gitignore` with a `*` in it to ignore all files, which would also affect `git add`, but I haven't found I need that.

```sh
git config status.showUntrackedFiles no
```

Checkout the main branch. _Will overwrite files in home directory if needed._

```sh
git checkout -f main
```
