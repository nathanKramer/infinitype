module Texts.Git exposing (..)


corpus =
    { monosize = 0.5
    , name = "Git"
    , words = words
    }


words =
    """git status
git commit -am ""
git push -u origin main
git checkout main
git switch main
git checkout -b new-branch
git log
git pull
git rebase -i main
git reset --hard origin/main
git reflog
git add
git push --delete origin feat/1
git branch -d
git commit --amend --reuse-message=HEAD
git reset HEAD~1
git commit --fixup
git rebase -i --autosquash
git push --force-with-lease
develop
main
master
qa
uat
trunk
feat
bug
fix
"""
