
# upgrade version

At the moment in package.yaml.template

# Record changes in changelog.md

# Upload to hackage

make sdist


# Add github release

Goto releases make new draft release.

In here you can just paste what you wrote in the chagnelog

# Make debian image

This is broken at the moment due to the pocketsphinx dependency.
We'll fix it once it works.

make ubuntu-release
