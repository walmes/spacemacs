#=======================================================================
# Spacemacs Installation and Configuration.

# Start a Git Project.
mkdir ~/GitLab/spacemacs/
cd ~/GitLab/spacemacs/
git init
git status

# Remove any old file/dir.
rm -rf ~/.emacs.d/
rm ~/.emacs
rm ~/.spacemacs

# Clone spacemacs from github.
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# Open Emacs to trigger the default installation.
emacs &

# Copy spacemacs init file.
cp -v ~/.spacemacs dotspacemacs.el
git commit -m "Add .spacemacs original file."

# Get a picture of the elpa/ directory.
tree --du ~/.emacs.d/elpa/ > dotemacsd-elpa.txt
git status
git difftool
git add dotemacsd-elpa.txt
git commit -m ".emacs.d/elpa/ after ** layer/package."

# Visit http://spacemacs.org/layers/LAYERS.html to see the layer list.
# Lets add the ess layer to use R. Open .spacemacs and add ess to the
# `dotspacemacs-configuration-layers` list.

mkdir wz-ess/
mkdir wz-misc/
mkdir wz-polymode/

git add wz-ess/packages.el
git add wz-misc/packages.el
git add wz-polymode/packages.el
git commit -m "Add wz-*** layer with *** package."

# My functions in wz-misc.
cp -vrf ~/GitLab/emacs/functions.el\
   ~/.emacs.d/private/wz-misc/funcs.el

# Add entries for layers, move file and restart Emacs.
geany dotspacemacs.el &
git add dotspacemacs.el
git commit -m "Add *** layer."
cp -v dotspacemacs.el ~/.spacemacs
cp -vrf wz-ess/ ~/.emacs.d/private/
cp -vrf wz-misc/ ~/.emacs.d/private/
cp -vrf wz-polymode/ ~/.emacs.d/private/

# Use an stable (bug free) version of polymode.
rm -vrf ~/.emacs.d/elpa/polymode-*     # recent from elpa
cp -vrf ~/dotemacs.d/elpa/polymode-*\  # old version
   ~/.emacs.d/elpa/
