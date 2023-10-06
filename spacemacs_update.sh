#-----------------------------------------------------------------------
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2023-out-05 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
# Enable personal configuration on Spacemacs.

# Removes all personal configuration.
rm -vrf ~/emacs-configs/spacemacs/private

#-----------------------------------------------------------------------
# `ess` layer ----------------------------------------------------------

# Delete layer `ess`.
rm -vrf ~/emacs-configs/spacemacs/private/private/ess

# Downloads `electric-spacing-r.el`.
mkdir -p ./ess/local/electric-spacing-r
wget "https://raw.githubusercontent.com/walmes/electric-spacing/master/electric-spacing-r.el" -O ./ess/local/electric-spacing-r/electric-spacing-r.el

# Downloads `essh.el`.
mkdir -p ./ess/local/essh
wget "https://www.emacswiki.org/emacs/download/essh.el" -O ./ess/local/essh/essh.el

# Copies `ess` to `private`.
cp -vrf ess ~/emacs-configs/spacemacs/private/private

#-----------------------------------------------------------------------
# `polymode` layer -----------------------------------------------------

# Copies `polymode` to `private`.
rm -vrf ~/emacs-configs/spacemacs/private/private/polymode
cp -vrf polymode ~/emacs-configs/spacemacs/private/private

#-----------------------------------------------------------------------
# `misc` layer ---------------------------------------------------------

# Copies `misc` to `private`.
rm -vrf ~/emacs-configs/spacemacs/private/private/misc
cp -vrf misc ~/emacs-configs/spacemacs/private/private

#-----------------------------------------------------------------------
# `funk` layer ---------------------------------------------------------

# Downloads `funcs.el`.
mkdir -p ./funk
wget "https://raw.githubusercontent.com/walmes/doom-emacs/master/funcs.el" -O ./funk/funcs.el
# cp -v ~/.doom.d/funcs.el ./funk/funcs.el

# Copies `funk` to `private`.
rm -vrf ~/emacs-configs/spacemacs/private/private/funk
cp -vrf funk ~/emacs-configs/spacemacs/private/private

#-----------------------------------------------------------------------
# Snippets -------------------------------------------------------------

# Copy snippets.
cp -rfv ~/.doom.d/snippets ~/emacs-configs/spacemacs

#-----------------------------------------------------------------------
# Last step ------------------------------------------------------------

# Transfer the main file.
cp -v dotspacemacs.el ~/.spacemacs

#-----------------------------------------------------------------------
