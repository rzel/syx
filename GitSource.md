# Why? #

The VCS of Syx is now Git. Google SVN gave us a lot of troubles with SSL when committing changes. The project is now hosted at http://git.berlios.de.

Please read the [crash courses](http://git.or.cz/) for more details.

# Command-Line Access #

If you plan to make changes, please [register here](http://developer.berlios.de) then [contact the administrator](mailto:lethalman88@gmail.com) to gain push access. Use this command to clone the code as yourself using SSH:

```
# Project members authenticate over SSH to allow committing changes.
git clone ssh://developername@git.berlios.de/gitroot/syx
```

Use this command to anonymously clone the latest project source code:

```
# Non-members may clone a working copy anonymously over GIT.
git clone git://git.berlios.de/syx
```

# Web Access #

You can see the tree, branches, tags and download a tarball snapshot of Syx at https://git.berlios.de/cgi-bin/gitweb.cgi?p=syx;a=summary