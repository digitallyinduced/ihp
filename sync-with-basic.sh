git remote add basic git@github.com:digitallyinduced/ihp.git
git config remote.basic.pushurl "this remote is readonly"
git fetch basic
git merge basic/master
git remote rm basic
git push pro