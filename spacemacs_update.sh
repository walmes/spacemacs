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

# /home/walmes/emacs-configs/
# └── spacemacs/
#     ├── assets/
#     ├── init.el
#     └── private/
#         ├── ess/             # R.
#         ├── funk/            # My functions.
#         ├── local/
#         ├── misc/            # Miscellania.
#         └── polymode/        # RMarkdown.

# Creates variables.
PRIVATE_DIR=/home/walmes/emacs-configs/spacemacs/private
SOURCE_DIR=/home/walmes/Projects/spacemacs

# DANGER: Removes all personal configuration. Make a backup.
rm -vrf $PRIVATE_DIR

#-----------------------------------------------------------------------
# `ess` layer ----------------------------------------------------------

# Delete layer `ess`.
rm -vrf $PRIVATE_DIR/ess

# Downloads `electric-spacing-r.el`.
mkdir -p $PRIVATE_DIR/ess/local/electric-spacing-r
wget "https://raw.githubusercontent.com/walmes/electric-spacing/master/electric-spacing-r.el" -O $PRIVATE_DIR/ess/local/electric-spacing-r/electric-spacing-r.el
# cp -v ~/Projects/electric-spacing/electric-spacing-r.el $PRIVATE_DIR/ess/local/electric-spacing-r/electric-spacing-r.el

# Downloads `essh.el`.
mkdir -p $PRIVATE_DIR/ess/local/essh
wget "https://www.emacswiki.org/emacs/download/essh.el" -O $PRIVATE_DIR/ess/local/essh/essh.el

# Copies `ess` to `private`.
tree -L 2 $SOURCE_DIR/ess
cp -vrf $SOURCE_DIR/ess/config.el $SOURCE_DIR/ess/packages.el $PRIVATE_DIR/ess

#-----------------------------------------------------------------------
# `polymode` layer -----------------------------------------------------

# Copies `polymode` to `private`.
rm -vrf $PRIVATE_DIR/polymode
tree -L 1 $SOURCE_DIR/polymode
cp -vrf $SOURCE_DIR/polymode $PRIVATE_DIR

#-----------------------------------------------------------------------
# `misc` layer ---------------------------------------------------------

# Copies `misc` to `private`.
rm -vrf $PRIVATE_DIR/misc
tree -L 1 $SOURCE_DIR/polymode
cp -vrf $SOURCE_DIR/misc $PRIVATE_DIR

#-----------------------------------------------------------------------
# `funk` layer ---------------------------------------------------------

# Downloads `funcs.el`.
# mkdir -p $PRIVATE_DIR/funk
# wget "https://raw.githubusercontent.com/walmes/doom-emacs/master/funcs.el" -O ./funk/funcs.el
# cp -v ~/.doom.d/funcs.el $PRIVATE_DIR/funk

# Copies `funk` to `private`.
rm -vrf $PRIVATE_DIR/funk
tree -L 1 $SOURCE_DIR/funk
cp -vrf $SOURCE_DIR/funk $PRIVATE_DIR

#-----------------------------------------------------------------------
# Snippets -------------------------------------------------------------

# My snippets.
tree -L 1 ~/.doom.d/snippets

# Default snippets.
tree -L 1 ~/emacs-configs/spacemacs/snippets

# Remove default snippets.
# rm -vrf ~/emacs-configs/spacemacs/snippets

# Copy my snippets.
cp -rfv ~/.doom.d/snippets ~/emacs-configs/spacemacs

#-----------------------------------------------------------------------
# Elpy -----------------------------------------------------------------

# # Copies `elpy` to `private`.
# rm -vrf $PRIVATE_DIR/elpy
# tree -L 1 $SOURCE_DIR/elpy
# cp -vrf elpy ~/emacs-configs/spacemacs/private/private
# cp -vrf $SOURCE_DIR/elpy $PRIVATE_DIR

# # TODO: enable lsp using mspyls.

#-----------------------------------------------------------------------
# Last step ------------------------------------------------------------

# Transfer the main file.
cp -v dotspacemacs.el ~/.spacemacs

# Check the results.
tree -L 1 /home/walmes/emacs-configs/spacemacs
tree -L 2 $PRIVATE_DIR

#-----------------------------------------------------------------------
